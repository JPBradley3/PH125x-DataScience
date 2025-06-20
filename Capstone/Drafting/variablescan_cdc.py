import os
import re
from pathlib import Path
import pandas as pd
from bs4 import BeautifulSoup
import PyPDF2
import pdfplumber

class BRFSSCodebookScraper:
    def __init__(self, base_path):
        self.base_path = Path(base_path)
        self.html_path = self.base_path / "html"
        self.pdf_path = self.base_path / "pdf"
        self.variables = []
        
    def scrape_all(self):
        print("Starting BRFSS codebook scraping...")
        
        if self.html_path.exists():
            print(f"\nScraping HTML files from: {self.html_path}")
            self.scrape_html_files()
        
        if self.pdf_path.exists():
            print(f"\nScraping PDF files from: {self.pdf_path}")
            self.scrape_pdf_files()
        
        if self.variables:
            df = pd.DataFrame(self.variables)
            df = df.drop_duplicates(subset=['variable_name'], keep='first')
            df.to_csv(self.base_path / "brfss_variables.csv", index=False)
            print(f"\nSaved {len(df)} unique variables to brfss_variables.csv")
            return df
        else:
            print("No variables found!")
            return None
    
    def scrape_html_files(self):
        html_files = list(self.html_path.glob("*.html")) + list(self.html_path.glob("*.htm"))
        
        for html_file in html_files:
            print(f"Processing: {html_file.name}")
            try:
                with open(html_file, 'r', encoding='utf-8', errors='ignore') as f:
                    soup = BeautifulSoup(f, 'html.parser')
                    self.extract_variables_from_html(soup, html_file.name)
            except Exception as e:
                print(f"Error processing {html_file.name}: {e}")
    
    def extract_variables_from_html(self, soup, filename):
        tables = soup.find_all('table')
        for table in tables:
            rows = table.find_all('tr')
            for row in rows:
                cells = row.find_all(['td', 'th'])
                if len(cells) >= 2:
                    potential_var = cells[0].get_text(strip=True)
                    if re.match(r'^[A-Z_][A-Z0-9_]{0,31}$', potential_var):
                        description = cells[1].get_text(strip=True)
                        if description and len(description) > 3:
                            self.variables.append({
                                'variable_name': potential_var,
                                'description': description,
                                'source_file': filename,
                                'source_type': 'html'
                            })
    
    def scrape_pdf_files(self):
        pdf_files = list(self.pdf_path.glob("*.pdf"))
        
        for pdf_file in pdf_files:
            print(f"Processing: {pdf_file.name}")
            try:
                self.extract_with_pdfplumber(pdf_file)
            except Exception as e:
                print(f"Error processing {pdf_file.name}: {e}")
    
    def extract_with_pdfplumber(self, pdf_file):
        with pdfplumber.open(pdf_file) as pdf:
            for page_num, page in enumerate(pdf.pages):
                text = page.extract_text()
                if text:
                    self.extract_variables_from_text(text, pdf_file.name, page_num + 1)
    
    def extract_variables_from_text(self, text, filename, page_num=None):
        # Enhanced patterns to capture full descriptions
        lgbt_patterns = [
            r'(SXORIENT|SEXORIEN|TRNSGNDR|TRANSGEN)\s*:?\s*([^\n]+(?:\n[^\n\w]*[^\n]+)*)',
            r'Variable Name:\s*(SXORIENT|SEXORIEN|TRNSGNDR|TRANSGEN)\s*\n([^\n]+(?:\n[^\n\w]*[^\n]+)*)',
            r'([A-Z_]*(?:SEX|ORIENT|TRANS|GENDER)[A-Z0-9_]*)\s+([A-Za-z][^\n]{20,300})'
        ]
        
        for pattern in lgbt_patterns:
            matches = re.findall(pattern, text, re.MULTILINE | re.DOTALL)
            for var_name, description in matches:
                var_name = var_name.strip()
                description = re.sub(r'\s+', ' ', description.strip())
                
                # Filter out common non-descriptive text
                if any(skip in description.lower() for skip in ['question prologue', 'calculated by', 'go to']):
                    continue
                
                lgbt_keywords = ['gay', 'lesbian', 'bisexual', 'sexual orientation', 'transgender', 'lgb', 'lgbt', 'straight', 'heterosexual']
                is_lgbt = any(keyword in description.lower() for keyword in lgbt_keywords) or var_name.upper() in ['SXORIENT', 'SEXORIEN', 'TRNSGNDR', 'TRANSGEN']
                
                if (3 <= len(var_name) <= 32 and len(description) >= 5):
                    var_dict = {
                        'variable_name': var_name,
                        'description': description,
                        'source_file': filename,
                        'source_type': 'pdf_text',
                        'lgbt_related': is_lgbt
                    }
                    if page_num:
                        var_dict['page'] = page_num
                    
                    self.variables.append(var_dict)

def main():
    base_path = r"C:\Users\Parker\Documents\DataScienceClass\Capstone\PH125x-DataScience\Capstone\Drafting\brfss_codebooks"
    
    scraper = BRFSSCodebookScraper(base_path)
    df = scraper.scrape_all()
    
    if df is not None:
        print("\nFirst 10 variables found:")
        print(df.head(10))
        print(f"\nTotal unique variables: {len(df)}")
        
        df.to_excel(Path(base_path) / "brfss_variables.xlsx", index=False)
        print("Also saved as brfss_variables.xlsx")

if __name__ == "__main__":
    main()