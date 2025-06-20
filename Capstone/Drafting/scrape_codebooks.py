import os
import re
import csv

from typing import List, Dict, Optional, Any

def search_in_text_file(filepath: str, keywords: List[str]) -> List[Dict[str, Any]]:
    """
    Searches for keywords in a plain text file, line by line.
    Returns a list of dictionaries, each containing found item details.
    """
    found_items = []
    try:
        with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
            for line_num, line in enumerate(f, 1):
                for keyword in keywords:
                    # Using regex for case-insensitive search and whole word matching
                    if re.search(r'\b' + re.escape(keyword) + r'\b', line, re.IGNORECASE):
                        found_items.append({
                            "file": filepath,
                            "line_number": line_num,
                            "line_content": line.strip(),
                            "keyword_found": keyword
                        })
                        break # Found a keyword in this line, move to the next line
    except Exception as e:
        print(f"Error reading text file {filepath}: {e}")
    return found_items

def search_in_csv_file(filepath: str, keywords: List[str], search_columns: Optional[List[str]] = None) -> List[Dict[str, Any]]:
    """
    Searches for keywords in a CSV file.
    If search_columns (list of column names) is provided, only those are searched.
    Otherwise, all columns are searched.
    Returns a list of dictionaries, each containing found item details.
    """
    found_items = []
    try:
        with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
            reader = csv.reader(f)
            header = next(reader, None)
            if not header:
                print(f"Warning: CSV file {filepath} is empty or has no header.")
                return found_items

            for row_num, row in enumerate(reader, 2): # row_num starts from 2 (data begins after header)
                if len(row) != len(header): # Handle potentially malformed rows
                    print(f"Warning: Skipping malformed row {row_num} in {filepath} due to column count mismatch.")
                    continue
                
                for col_idx, cell_value in enumerate(row):
                    current_column_name = header[col_idx]
                    if search_columns and current_column_name not in search_columns:
                        continue # Skip if column is not in the specified list

                    for keyword in keywords:
                        if re.search(r'\b' + re.escape(keyword) + r'\b', str(cell_value), re.IGNORECASE):
                            found_items.append({
                                "file": filepath,
                                "row_number": row_num,
                                "column_name": current_column_name,
                                "cell_value": str(cell_value).strip(),
                                "keyword_found": keyword,
                                "full_row_context": dict(zip(header, row)) # Provides context of the entire row
                            })
                            break # Found keyword in this cell, move to next cell in this row
                    else: # If inner loop (keywords) didn't break
                        continue # Check next cell
                    break # If inner loop did break (keyword found), move to next row
    except Exception as e:
        print(f"Error reading CSV file {filepath}: {e}")
    return found_items

def find_relevant_codes_from_codebooks(
    codebook_filepaths: List[str], 
    search_keywords: List[str], 
    csv_search_specific_columns: Optional[List[str]] = None
) -> List[Dict[str, Any]]:
    """
    Main function to iterate through codebook files and search for keywords.

    Args:
        codebook_filepaths (list): A list of strings, where each string is a path to a codebook file.
        search_keywords (list): A list of strings, keywords to search for.
        csv_search_specific_columns (list, optional): A list of column names to search within CSV files.
                                                     If None, all columns in CSVs are searched.
    Returns:
        list: A list of dictionaries, where each dictionary details a found relevant item.
    """
    all_found_items = []
    for filepath in codebook_filepaths:
        if not os.path.exists(filepath):
            print(f"Warning: File not found at {filepath}. Skipping.")
            continue

        print(f"\n--- Searching in: {os.path.basename(filepath)} ---")
        _, extension = os.path.splitext(filepath.lower())
        
        if extension == '.txt':
            all_found_items.extend(search_in_text_file(filepath, search_keywords))
        elif extension == '.csv':
            all_found_items.extend(search_in_csv_file(filepath, search_keywords, csv_search_specific_columns))
        # To extend for other formats:
        # elif extension == '.pdf':
        #     print(f"PDF searching for {filepath} requires a PDF library (e.g., PyPDF2, pdfplumber).")
        #     # Example: all_found_items.extend(search_in_pdf_file(filepath, search_keywords))
        # elif extension in ['.xls', '.xlsx']:
        #     print(f"Excel searching for {filepath} requires an Excel library (e.g., pandas, openpyxl).")
        #     # Example: all_found_items.extend(search_in_excel_file(filepath, search_keywords))
        else:
            print(f"Unsupported file type for automatic search: {filepath}. Skipping.")
            
    return all_found_items

# --- How to use the script ---
if __name__ == "__main__":
    # 1. Define the base directory where your BRFSS data (and hopefully codebooks) are stored.
    base_brfss_data_dir = r"C:\Users\Parker\Documents\DataScienceClass\Capstone\PH125x-DataScience\Capstone\Drafting\BRFSS_Data"

    my_codebook_files = []
    supported_extensions = ('.txt', '.csv') # Add other extensions if your functions support them

    if os.path.exists(base_brfss_data_dir) and os.path.isdir(base_brfss_data_dir):
        # Iterate through year directories (e.g., "2014", "2015")
        for year_dir_name in os.listdir(base_brfss_data_dir):
            year_dir_path = os.path.join(base_brfss_data_dir, year_dir_name)
            if os.path.isdir(year_dir_path):
                # Look for codebook files within each year directory
                for filename in os.listdir(year_dir_path):
                    if filename.lower().endswith(supported_extensions):
                        # You might want to add more specific checks if codebooks have a pattern
                        # For example, if they always contain "codebook" or "layout" in their name:
                        # if "codebook" in filename.lower() or "layout" in filename.lower() or "cod" in filename.lower():
                        my_codebook_files.append(os.path.join(year_dir_path, filename))
    else:
        print(f"Warning: The specified BRFSS data directory does not exist or is not a directory: {base_brfss_data_dir}")

    if not my_codebook_files:
        print(f"No codebook files ({', '.join(supported_extensions)}) found in {base_brfss_data_dir} or its year subdirectories.")
        print("Please ensure codebooks are present and have supported extensions, or manually list them.")

    # 2. Define the keywords you want to search for.
    #    These should be terms that help identify SOGI variables, codes, or descriptions.
    my_sogi_keywords = [
        "sexual orientation", "sexorient", "sexorien", "_sexori1", "sxorient",
        "gender identity", "transgender", "trnsgndr", "_trngrnd", "genid_describe",
        "gay", "lesbian", "bisexual", "straight", "heterosexual", "queer",
        "male", "female", "sex", "birthsex", "sex1", "sexvar",
        "lgbt", "sogi",
        "somale", "sofemale", # Variables related to sex of partners
        "cisgender"
        # You can also add specific numeric codes if you're looking for their definitions,
        # e.g., "1:", "2:", if codes are often listed like "1: Yes", "2: No"
    ]
    
    # 3. (Optional) For CSV files, if you want to search only in specific columns,
    #    list their names here. If set to None, all columns will be searched.
    #    Column names are case-sensitive as they appear in the CSV header.
    #    Example: my_csv_columns_to_search = ["Variable Name", "Label", "Description", "Values"]
    my_csv_columns_to_search = None 
    # my_csv_columns_to_search = ["VARIABLE_NAME", "DESCRIPTION", "CODE_VALUES"] # Example

    if not my_codebook_files:
        print("Please provide a list of codebook file paths in 'my_codebook_files'.")
    else:
        relevant_findings = find_relevant_codes_from_codebooks(
            my_codebook_files, 
            my_sogi_keywords,
            my_csv_columns_to_search
        )
        
        if relevant_findings:
            print("\n\n--- Summary of Found Relevant Items ---")
            for item in relevant_findings:
                if "line_number" in item: # From text file
                    print(f"File: {os.path.basename(item['file'])}")
                    print(f"  Keyword: '{item['keyword_found']}'")
                    print(f"  Line {item['line_number']}: {item['line_content']}")
                elif "row_number" in item: # From CSV file
                    print(f"File: {os.path.basename(item['file'])}")
                    print(f"  Keyword: '{item['keyword_found']}'")
                    print(f"  Row {item['row_number']}, Column '{item['column_name']}': {item['cell_value']}")
                    print(f"    Full Row Context: {item['full_row_context']}")
                print("-" * 30)
            
            # Optionally, save to a file
            # with open("relevant_sogi_codes_summary.txt", "w", encoding="utf-8") as outfile:
            #     for item in relevant_findings:
            #         outfile.write(str(item) + "\n")
            # print("\nSummary also saved to relevant_sogi_codes_summary.txt")

        else:
            print("\nNo relevant items found with the given keywords in the specified text/CSV files.")

    print("\nScript execution finished.")
    print("Tip: For PDF or Excel codebooks, you'll need to add specific functions using libraries")
    print("like 'PyPDF2' or 'pdfplumber' for PDFs, and 'pandas' or 'openpyxl' for Excel files.")
