import pandas as pd
import numpy as np
from pathlib import Path
import re

class LGBTVariableExtractor:
    def __init__(self, excel_path):
        self.excel_path = Path(excel_path)
        self.df = pd.read_excel(excel_path)
        
        # LGBT-related keywords and patterns
        self.lgbt_keywords = {
            'sexual_orientation': [
                'sexual orientation', 'gay', 'lesbian', 'bisexual', 'homosexual',
                'heterosexual', 'straight', 'same-sex', 'same sex', 'attracted to',
                'sexual identity', 'sexual preference', 'lgb', 'orientation'
            ],
            'gender_identity': [
                'transgender', 'gender identity', 'trans', 'gender', 'birth sex',
                'assigned sex', 'identify as', 'cisgender', 'non-binary', 'nonbinary',
                'gender expression', 'biological sex', 'natal sex'
            ],
            'sexual_behavior': [
                'sex with men', 'sex with women', 'msm', 'wsw', 'sexual partner',
                'sexual behavior', 'sexual activity', 'intimate partner'
            ],
            'discrimination': [
                'discrimination', 'treated unfairly', 'harassed', 'bullied',
                'stigma', 'prejudice', 'bias', 'treated differently'
            ],
            'healthcare': [
                'healthcare', 'health care', 'doctor', 'provider', 'coverage',
                'insurance', 'hormone', 'hrt', 'prep', 'hiv', 'std', 'sti'
            ],
            'mental_health': [
                'depression', 'anxiety', 'mental health', 'suicide', 'suicidal',
                'stress', 'support', 'counseling', 'therapy'
            ],
            'social_support': [
                'family support', 'acceptance', 'coming out', 'closeted',
                'community', 'social support', 'isolation'
            ]
        }
        
        # Common BRFSS variable patterns for LGBT-related questions
        self.variable_patterns = [
            r'SEX\d*',          # Sex/gender variables
            r'SEXORIEN\d*',     # Sexual orientation
            r'SXORIENT\d*',     # Alternative sexual orientation
            r'TRNSGNDR\d*',     # Transgender
            r'SOMALE\d*',       # Sex of male partners
            r'SOFEMALE\d*',     # Sex of female partners
            r'GENHLTH\d*',      # General health
            r'MENTHLTH\d*',     # Mental health
            r'HLTHPLN\d*',      # Health plan
            r'MEDCOST\d*',      # Medical cost
            r'CHECKUP\d*',      # Checkup
            r'CVDINFR\d*',      # Various health conditions
            r'CVDCRHD\d*',
            r'CVDSTRK\d*',
            r'ASTHMA\d*',
            r'CHCSCNCR\d*',
            r'CHCOCNCR\d*',
            r'CHCCOPD\d*',
            r'ADDEPEV\d*',      # Depression
            r'CHCKIDNY\d*',
            r'DIABETE\d*',
            r'MARITAL\d*',      # Marital status
            r'EDUCA\d*',        # Education
            r'EMPLOY\d*',       # Employment
            r'INCOME\d*',       # Income
            r'HIVTST\d*',       # HIV testing
            r'HIVRISK\d*'       # HIV risk
        ]
    
    def extract_lgbt_variables(self):
        """Extract variables that could be useful for LGBT classification"""
        lgbt_vars = []
        
        # Convert descriptions to lowercase for matching
        self.df['description_lower'] = self.df['description'].str.lower()
        
        # Method 1: Keyword matching in descriptions
        for category, keywords in self.lgbt_keywords.items():
            for _, row in self.df.iterrows():
                desc_lower = str(row['description_lower'])
                if any(keyword in desc_lower for keyword in keywords):
                    lgbt_vars.append({
                        'variable_name': row['variable_name'],
                        'description': row['description'],
                        'category': category,
                        'match_type': 'keyword',
                        'source_file': row.get('source_file', 'unknown')
                    })
        
        # Method 2: Pattern matching on variable names
        for _, row in self.df.iterrows():
            var_name = str(row['variable_name'])
            for pattern in self.variable_patterns:
                if re.match(pattern, var_name):
                    lgbt_vars.append({
                        'variable_name': row['variable_name'],
                        'description': row['description'],
                        'category': 'pattern_match',
                        'match_type': 'variable_pattern',
                        'source_file': row.get('source_file', 'unknown')
                    })
        
        # Convert to DataFrame and remove duplicates
        lgbt_df = pd.DataFrame(lgbt_vars).drop_duplicates(subset=['variable_name'])
        
        return lgbt_df
    
    def extract_demographic_variables(self):
        """Extract demographic variables that might be correlated with LGBT status"""
        demo_keywords = {
            'age': ['age', 'birth', 'born'],
            'race_ethnicity': ['race', 'ethnicity', 'hispanic', 'white', 'black', 
                              'asian', 'native american', 'pacific islander'],
            'education': ['education', 'school', 'grade', 'degree', 'college'],
            'income': ['income', 'money', 'earn', 'salary', 'wage'],
            'employment': ['employ', 'work', 'job', 'occupation'],
            'geography': ['state', 'county', 'urban', 'rural', 'city', 'metro'],
            'household': ['household', 'living', 'reside', 'home'],
            'relationship': ['marital', 'married', 'partner', 'spouse', 'relationship']
        }
        
        demo_vars = []
        
        for category, keywords in demo_keywords.items():
            for _, row in self.df.iterrows():
                desc_lower = str(row['description']).lower()
                if any(keyword in desc_lower for keyword in keywords):
                    demo_vars.append({
                        'variable_name': row['variable_name'],
                        'description': row['description'],
                        'category': category,
                        'source_file': row.get('source_file', 'unknown')
                    })
        
        return pd.DataFrame(demo_vars).drop_duplicates(subset=['variable_name'])
    
    def extract_health_outcome_variables(self):
        """Extract health outcome variables often studied in LGBT health research"""
        health_keywords = {
            'mental_health': ['depression', 'anxiety', 'mental', 'emotional', 
                            'psychological', 'stress', 'suicide'],
            'substance_use': ['alcohol', 'drink', 'smoke', 'tobacco', 'drug', 
                            'substance', 'marijuana', 'opioid'],
            'sexual_health': ['hiv', 'aids', 'std', 'sti', 'sexually transmitted',
                            'condom', 'safe sex', 'sexual health'],
            'chronic_conditions': ['diabetes', 'heart', 'cancer', 'asthma', 'copd',
                                 'kidney', 'arthritis', 'chronic'],
            'healthcare_access': ['insurance', 'coverage', 'doctor', 'physician',
                                'healthcare', 'medical care', 'checkup', 'cost'],
            'preventive_care': ['screening', 'vaccine', 'immunization', 'preventive',
                              'mammogram', 'colonoscopy', 'flu shot'],
            'overall_health': ['general health', 'overall health', 'health status',
                             'quality of life', 'physical health', 'health days']
        }
        
        health_vars = []
        
        for category, keywords in health_keywords.items():
            for _, row in self.df.iterrows():
                desc_lower = str(row['description']).lower()
                if any(keyword in desc_lower for keyword in keywords):
                    health_vars.append({
                        'variable_name': row['variable_name'],
                        'description': row['description'],
                        'category': category,
                        'source_file': row.get('source_file', 'unknown')
                    })
        
        return pd.DataFrame(health_vars).drop_duplicates(subset=['variable_name'])
    
    def create_analysis_ready_file(self, output_path=None):
        """Create a comprehensive file with all relevant variables for LGBT analysis"""
        if output_path is None:
            output_path = self.excel_path.parent / "lgbt_analysis_variables.xlsx"
        
        # Extract all categories
        lgbt_vars = self.extract_lgbt_variables()
        demo_vars = self.extract_demographic_variables()
        health_vars = self.extract_health_outcome_variables()
        
        # Create Excel writer
        with pd.ExcelWriter(output_path, engine='openpyxl') as writer:
            # Write each category to a separate sheet
            lgbt_vars.to_excel(writer, sheet_name='LGBT_Direct', index=False)
            demo_vars.to_excel(writer, sheet_name='Demographics', index=False)
            health_vars.to_excel(writer, sheet_name='Health_Outcomes', index=False)
            
            # Create a summary sheet
            summary_data = {
                'Category': ['LGBT Direct Variables', 'Demographic Variables', 'Health Outcome Variables'],
                'Count': [len(lgbt_vars), len(demo_vars), len(health_vars)],
                'Description': [
                    'Variables directly related to sexual orientation and gender identity',
                    'Demographic variables that may be correlated with LGBT status',
                    'Health outcomes often studied in LGBT health disparities research'
                ]
            }
            summary_df = pd.DataFrame(summary_data)
            summary_df.to_excel(writer, sheet_name='Summary', index=False)
            
            # Create a combined sheet with all variables and their relevance scores
            all_vars = self._score_variables_for_lgbt_relevance()
            all_vars.to_excel(writer, sheet_name='All_Variables_Scored', index=False)
        
        print(f"\nAnalysis file created: {output_path}")
        print(f"Total LGBT-related variables: {len(lgbt_vars)}")
        print(f"Total demographic variables: {len(demo_vars)}")
        print(f"Total health outcome variables: {len(health_vars)}")
        
        return output_path
    
    def _score_variables_for_lgbt_relevance(self):
        """Score all variables for their relevance to LGBT classification"""
        all_vars = self.df.copy()
        all_vars['lgbt_relevance_score'] = 0
        all_vars['relevance_reasons'] = ''
        
        for idx, row in all_vars.iterrows():
            score = 0
            reasons = []
            desc_lower = str(row['description']).lower()
            var_name = str(row['variable_name'])
            
            # Direct LGBT indicators (highest score)
            if any(term in desc_lower for term in ['sexual orientation', 'transgender', 
                                                   'gay', 'lesbian', 'bisexual']):
                score += 10
                reasons.append('direct_lgbt_indicator')
            
            # Sexual behavior indicators
            if any(term in desc_lower for term in ['sex with', 'sexual partner', 'msm', 'wsw']):
                score += 8
                reasons.append('sexual_behavior')
            
            # Key demographic variables
            if any(term in desc_lower for term in ['sex', 'gender', 'age', 'marital']):
                score += 5
                reasons.append('key_demographic')
            
            # Health disparities often studied in LGBT populations
            if any(term in desc_lower for term in ['hiv', 'mental health', 'depression', 
                                                   'suicide', 'discrimination']):
                score += 7
                reasons.append('lgbt_health_disparity')
            
            # Healthcare access
            if any(term in desc_lower for term in ['healthcare', 'insurance', 'doctor']):
                score += 4
                reasons.append('healthcare_access')
            
            # Variable name patterns
            if re.match(r'(SEX|TRNSGNDR|SXORIENT)', var_name):
                score += 6
                reasons.append('variable_pattern')
            
            all_vars.at[idx, 'lgbt_relevance_score'] = score
            all_vars.at[idx, 'relevance_reasons'] = ', '.join(reasons)
        
        # Sort by relevance score
        all_vars = all_vars.sort_values('lgbt_relevance_score', ascending=False)
        
        return all_vars

def main():
    # Path to your Excel file
    excel_path = r"C:\Users\Parker\Documents\DataScienceClass\Capstone\PH125x-DataScience\Capstone\Drafting\brfss_codebooks\brfss_variables.xlsx"
    
    # Create extractor
    extractor = LGBTVariableExtractor(excel_path)
    
    # Create comprehensive analysis file
    output_path = extractor.create_analysis_ready_file()
    
    # Also create a simple CSV of just the most relevant variables
    scored_vars = extractor._score_variables_for_lgbt_relevance()
    top_vars = scored_vars[scored_vars['lgbt_relevance_score'] > 0].head(50)
    top_vars.to_csv(
        Path(excel_path).parent / "top_lgbt_relevant_variables.csv", 
        index=False
    )
    print(f"\nTop 50 LGBT-relevant variables saved to: top_lgbt_relevant_variables.csv")

if __name__ == "__main__":
    main()