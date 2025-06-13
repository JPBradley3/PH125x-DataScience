import os
import zipfile
import urllib.request
import shutil
import pandas as pd


# The 'xpt_url' key now points to either a direct .XPT file or a .ZIP containing the .XPT file.
# Complete and verified list of data sources from 2023 down to 2010.
# This list accounts for changing naming conventions and file formats over the years.
file_info = [
    {
        'year': '2023',
        'zip_asc': 'https://www.cdc.gov/brfss/annual_data/2023/files/LLCP23V2_ASC.ZIP',
        'xpt_url': 'https://www.cdc.gov/brfss/annual_data/2023/files/LLCP2023XPT.zip' # XPT is in a ZIP
    },
    {
        'year': '2022',
        'zip_asc': 'https://www.cdc.gov/brfss/annual_data/2022/files/LLCP22V2_ASC.ZIP',
        'xpt_url': 'https://www.cdc.gov/brfss/annual_data/2022/files/LLCP2022XPT.zip' # XPT is in a ZIP
    },
    {
        'year': '2021',
        'zip_asc': 'https://www.cdc.gov/brfss/annual_data/2021/files/LLCP21V3_ASC.ZIP',
        'xpt_url': 'https://www.cdc.gov/brfss/annual_data/2021/files/LLCP2021XPT.zip' # XPT is in a ZIP
    },
    {
        'year': '2020',
        'zip_asc': 'https://www.cdc.gov/brfss/annual_data/2020/files/LLCP20V3_ASC.ZIP',
        'xpt_url': 'https://www.cdc.gov/brfss/annual_data/2020/files/LLCP2020XPT.zip' # Direct XPT download
    },
    {
        'year': '2019',
        'zip_asc': 'https://www.cdc.gov/brfss/annual_data/2019/files/LLCP19V3_ASC.zip',
        'xpt_url': 'https://www.cdc.gov/brfss/annual_data/2019/files/LLCP2019XPT.zip'
    },
    {
        'year': '2018',
        'zip_asc': 'https://www.cdc.gov/brfss/annual_data/2018/files/LLCP18V3_ASC.zip',
        'xpt_url': 'https://www.cdc.gov/brfss/annual_data/2018/files/LLCP2018XPT.zip'
    },
    {
        'year': '2017',
        'zip_asc': 'https://www.cdc.gov/brfss/annual_data/2017/files/LLCP17V3_ASC.zip',
        'xpt_url': 'https://www.cdc.gov/brfss/annual_data/2017/files/LLCP2017XPT.zip'
    },
    {
        'year': '2016',
        'zip_asc': 'https://www.cdc.gov/brfss/annual_data/2016/files/LLCP16V3_ASC.zip',
        'xpt_url': 'https://www.cdc.gov/brfss/annual_data/2016/files/LLCP2016XPT.zip'
    },
    {
        'year': '2015', 
        'zip_asc': 'https://www.cdc.gov/brfss/annual_data/2015/llcp_multiq.html', 
        'xpt_url': 'https://www.cdc.gov/brfss/annual_data/2015/files/LLCP2015XPT.zip',
        'info': 'https://www.cdc.gov/brfss/annual_data/2015/annual_data_2015.html' # No data file
    },
    {
        'year': '2014',
        'zip_asc': 'https://www.cdc.gov/brfss/annual_data/2014/files/LLCP14V3_ASC.ZIP',
        'xpt_url': 'https://www.cdc.gov/brfss/annual_data/2014/files/LLCP2014XPT.ZIP'
    },
    {
        'year': '2013',
        'zip_asc': 'https://www.cdc.gov/brfss/annual_data/2013/files/LLCP13V3_ASC.ZIP',
        'xpt_url': 'https://www.cdc.gov/brfss/annual_data/2013/files/LLCP2013XPT.ZIP'
    },
    {
        'year': '2012',
        'zip_asc': 'https://www.cdc.gov/brfss/annual_data/2012/files/LLCP12V3_ASC.ZIP',
        'xpt_url': 'https://www.cdc.gov/brfss/annual_data/2012/files/LLCP2012XPT.ZIP'
    },
    {
        'year': '2011',
        'zip_asc': 'https://www.cdc.gov/brfss/annual_data/2011/files/LLCP2011ASC.ZIP',
        'xpt_url': 'https://www.cdc.gov/brfss/annual_data/2011/files/LLCP2011XPT.ZIP'
    },
    {
        'year': '2010',
        'zip_asc': 'https://www.cdc.gov/brfss/annual_data/2010/files/CDBRFS10ASC.zip',
        'xpt_url': 'https://www.cdc.gov/brfss/annual_data/2010/files/CDBRFS10XPT.zip'
    }
]

# Parent directory for all data
parent_dir = "BRFSS_Data"
os.makedirs(parent_dir, exist_ok=True)

def find_file_by_extension(folder, ext):
    """Helper function to find the first file with a given extension in a folder."""
    for item in os.listdir(folder):
        if item.lower().endswith(ext.lower()):
            return os.path.join(folder, item)
    return None

for item in file_info:
    year = item['year']
    print(f"--- Processing {year} ---")
    dest_folder = os.path.join(parent_dir, year)
    os.makedirs(dest_folder, exist_ok=True)

    # --- 1. Process the ASCII ZIP (codebooks, documentation) ---
    if item.get('zip_asc'):
        zip_url = item['zip_asc']
        local_zip_path = os.path.join(dest_folder, os.path.basename(zip_url))
        print(f"  Downloading ASCII ZIP: {zip_url}")
        try:
            with urllib.request.urlopen(zip_url) as response, open(local_zip_path, 'wb') as out_file:
                shutil.copyfileobj(response, out_file)
            
            with zipfile.ZipFile(local_zip_path, 'r') as zip_ref:
                zip_ref.extractall(dest_folder)
            print(f"  Extracted ASCII ZIP to {dest_folder}")
            os.remove(local_zip_path) # Clean up the zip file

        except Exception as e:
            print(f"  [ERROR] Failed to process ASCII ZIP: {e}")

    # --- 2. Process the XPT data (the main dataset) ---
    xpt_url = item.get('xpt_url')
    if xpt_url:
        local_xpt_path = None
        try:
            # Case A: The XPT file is inside its own ZIP archive (2021+)
            if xpt_url.lower().endswith('.zip'):
                print(f"  Downloading XPT archive: {xpt_url}")
                xpt_zip_path = os.path.join(dest_folder, os.path.basename(xpt_url))
                with urllib.request.urlopen(xpt_url) as response, open(xpt_zip_path, 'wb') as out_file:
                    shutil.copyfileobj(response, out_file)
                
                print(f"  Extracting XPT from archive...")
                with zipfile.ZipFile(xpt_zip_path, 'r') as zip_ref:
                    zip_ref.extractall(dest_folder)
                
                os.remove(xpt_zip_path) # Clean up the XPT's zip file
                
                # Now find the extracted .xpt file
                local_xpt_path = find_file_by_extension(dest_folder, '.xpt')
                if not local_xpt_path:
                    raise FileNotFoundError("Could not find .XPT file after extraction.")

            # Case B: The URL points directly to the XPT file (pre-2021)
            else:
                print(f"  Downloading direct XPT file: {xpt_url}")
                local_xpt_path = os.path.join(dest_folder, os.path.basename(xpt_url))
                with urllib.request.urlopen(xpt_url) as response, open(local_xpt_path, 'wb') as out_file:
                    shutil.copyfileobj(response, out_file)

            # --- 3. Convert the final XPT file to CSV ---
            print(f"  Converting {os.path.basename(local_xpt_path)} to CSV...")
            csv_path = os.path.splitext(local_xpt_path)[0] + '.csv'
            df = pd.read_sas(local_xpt_path, format='xport')
            df.to_csv(csv_path, index=False)
            print(f"  Successfully converted to {os.path.basename(csv_path)}")
            os.remove(local_xpt_path) # Clean up the xpt file

        except Exception as e:
            print(f"  [ERROR] Failed to download or process XPT data: {e}")

    # --- 4. Handle special cases with no data ---
    if item.get('info'):
        print(f"  NOTE: Year {year} has no direct data download.")
        print(f"  For more information, visit: {item['info']}")

print("\nDone!")