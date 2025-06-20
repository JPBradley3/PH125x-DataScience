import os
import re
from pathlib import Path
import pandas as pd
from bs4 import BeautifulSoup
import PyPDF2
import pdfplumber
from collections import defaultdict

class BRFSSCodebookScraper:
    def __init__(self, base_path):
        self.base_path = Path(base_path)
        self.html_path = self.base_path / "html"
        self.pdf_path = self.base_path / "pdf"
        self.variables = []
        
    def scrape_all(self):
        """Main method to scrape all files"""
        print("Starting BRFSS codebook scraping...")
        
        # Scrape HTML files
        if self.html_path.exists():
            print(f"\nScraping HTML files from: {self.html_path}")
            self.scrape_html_files()
        
        # Scrape PDF files
        if self.pdf_path.exists():
            print(f"\nScraping PDF files from: {self.pdf_path}")
            self.scrape_pdf_files()
        
        # Convert to DataFrame and save
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
        """Scrape all HTML files in the html directory"""
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
        """Extract SAS variables from HTML content"""
        # Common patterns in BRFSS codebooks
        # Pattern 1: Look for tables with variable information
        tables = soup.find_all('table')
        for table in tables:
            rows = table.find_all('tr')
            for row in rows:
                cells = row.find_all(['td', 'th'])
                if len(cells) >= 2:
                    # Look for SAS variable patterns (usually uppercase, alphanumeric)
                    potential_var = cells[0].get_text(strip=True)
                    if re.match(r'^[A-Z_][A-Z0-9_]{0,31}$', potential_var):
                        description = cells[1].get_text(strip=True)
                        if description and len(description) > 3:  # Filter out empty descriptions
                            self.variables.append({
                                'variable_name': potential_var,
                                'description': description,
                                'source_file': filename,
                                'source_type': 'html'
                            })
        
        # Pattern 2: Look for definition lists or specific div patterns
        # Search for patterns like "Variable: VARNAME Description: ..."
        text_content = soup.get_text()
        var_pattern = r'(?:Variable|SAS Variable|SAS Name|Variable Name)[\s:]+([A-Z_][A-Z0-9_]{0,31})[\s\n]+(?:Description|Label|Question)[\s:]+([^\n]+)'
        matches = re.findall(var_pattern, text_content, re.IGNORECASE)
        
        for var_name, description in matches:
            self.variables.append({
                'variable_name': var_name,
                'description': description.strip(),
                'source_file': filename,
                'source_type': 'html'
            })
    
    def scrape_pdf_files(self):
        """Scrape all PDF files in the pdf directory"""
        pdf_files = list(self.pdf_path.glob("*.pdf"))
        
        for pdf_file in pdf_files:
            print(f"Processing: {pdf_file.name}")
            try:
                # Try pdfplumber first (better for tables)
                self.extract_with_pdfplumber(pdf_file)
            except Exception as e:
                print(f"pdfplumber failed for {pdf_file.name}: {e}")
                # Fallback to PyPDF2
                try:
                    self.extract_with_pypdf2(pdf_file)
                except Exception as e2:
                    print(f"PyPDF2 also failed for {pdf_file.name}: {e2}")
    
    def extract_with_pdfplumber(self, pdf_file):
        """Extract variables using pdfplumber (better for tables)"""
        with pdfplumber.open(pdf_file) as pdf:
            for page_num, page in enumerate(pdf.pages):
                # Extract tables
                tables = page.extract_tables()
                for table in tables:
                    for row in table:
                        if row and len(row) >= 2:
                            potential_var = str(row[0]).strip() if row[0] else ""
                            if re.match(r'^[A-Z_][A-Z0-9_]{0,31}$', potential_var):
                                description = str(row[1]).strip() if row[1] else ""
                                if description and len(description) > 3:
                                    self.variables.append({
                                        'variable_name': potential_var,
                                        'description': description,
                                        'source_file': pdf_file.name,
                                        'source_type': 'pdf',
                                        'page': page_num + 1
                                    })
                
                # Also extract from text
                text = page.extract_text()
                if text:
                    self.extract_variables_from_text(text, pdf_file.name, page_num + 1)
    
    def extract_with_pypdf2(self, pdf_file):
        """Extract variables using PyPDF2 (fallback method)"""
        with open(pdf_file, 'rb') as f:
            pdf_reader = PyPDF2.PdfReader(f)
            for page_num, page in enumerate(pdf_reader.pages):
                text = page.extract_text()
                self.extract_variables_from_text(text, pdf_file.name, page_num + 1)
    
    def extract_variables_from_text(self, text, filename, page_num=None):
        """Extract variables from text using regex patterns"""
        # More specific patterns for BRFSS codebooks
        patterns = [
            r'SAS Variable Name:\s*([A-Z_][A-Z0-9_]{0,31})\s*\n.*?(?:Description|Label):\s*([^\n]+)',
            r'Variable:\s*([A-Z_][A-Z0-9_]{0,31})\s*\n.*?([A-Z][a-z][^\n]{15,100})',
            r'^([A-Z_][A-Z0-9_]{2,31})\s+([A-Z][a-z][^\n]{20,100})$'
        ]
        
        for pattern in patterns:
            matches = re.findall(pattern, text, re.MULTILINE | re.DOTALL)
            for var_name, description in matches:
                var_name = var_name.strip()
                description = re.sub(r'\s+', ' ', description.strip())
                
                # Stricter validation
                if (3 <= len(var_name) <= 32 and
                    var_name.startswith(('_', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z')) and
                    20 <= len(description) <= 200 and
                    not any(word in description.lower() for word in ['hidden', 'blank', 'missing', 'page', 'section'])):
                    
                    var_dict = {
                        'variable_name': var_name,
                        'description': description,
                        'source_file': filename,
                        'source_type': 'pdf_text'
                    }
                    if page_num:
                        var_dict['page'] = page_num
                    
                    self.variables.append(var_dict)

def main():
    # Set the base path
    base_path = r"C:\Users\Parker\Documents\DataScienceClass\Capstone\PH125x-DataScience\Capstone\Drafting\brfss_codebooks"
    
    # Create scraper instance
    scraper = BRFSSCodebookScraper(base_path)
    
    # Scrape all files
    df = scraper.scrape_all()
    
    if df is not None:
        print("\nFirst 10 variables found:")
        print(df.head(10))
        print(f"\nTotal unique variables: {len(df)}")
        
        # Save additional formats if needed
        df.to_excel(Path(base_path) / "brfss_variables.xlsx", index=False)
        print("Also saved as brfss_variables.xlsx")

if __name__ == "__main__":
    main()