#############################################################################################################################
#############################################################################################################################
##
##                         ---------------------------------
##                            U.S. LGBT Emigration Study
##                         ---------------------------------
##
##  Name:             James P. Bradley III
##  Class:            PH125.9x Data Science
##  Assignment:       Capstone Project
##  Institute:        Harvard Online
##
##  Description:      This script processes and visualizes data from the
##                    CDC's BRFSS survey datasets on LGBT demographics and
##                    transgender population estimates.
##
#############################################################################################################################
#############################################################################################################################

#############################################################################################################################
#############################################################################################################################
##
##                         ---------------------------------
##                                   Bibliography
##                         ---------------------------------
##
##    U.S. Census Household Pulse Survey
##
##    • Household Pulse Survey Main Page
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2025/topical/HTOPS_HPS_2502%20CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2024/topical/HPS_DECEMBER2024_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2024/topical/HPS_OCTOBER2024_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2024/cycle09/HPS_Phase4-2Cycle09_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2024/cycle08/HPS_Phase4-2Cycle08_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2024/cycle07/HPS_Phase4-1Cycle07_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2024/cycle06/HPS_Phase4-1Cycle06_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2024/cycle05/HPS_Phase4-1Cycle05_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2024/cycle04/HPS_Phase4-1Cycle04_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2024/cycle03/HPS_Phase4Cycle03_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2024/cycle02/HPS_Phase4Cycle02_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2024/cycle01/HPS_Phase4Cycle01_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2023/wk63/HPS_Week63_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2023/wk62/HPS_Week62_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2023/wk61/HPS_Week61_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2023/wk60/HPS_Week60_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2023/wk59/HPS_Week59_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2023/wk58/HPS_Week58_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2023/wk57/HPS_Week57_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2023/wk56/HPS_Week56_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2023/wk55/HPS_Week55_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2023/wk54/HPS_Week54_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2023/wk53/HPS_Week53_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2023/wk52/HPS_Week52_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2022/wk51/HPS_Week51_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2022/wk50/HPS_Week50_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2022/wk49/HPS_Week49_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2022/wk48/HPS_Week48_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2022/wk47/HPS_Week47_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2022/wk46/HPS_Week46_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2022/wk45/HPS_Week45_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2022/wk44/HPS_Week44_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2022/wk43/HPS_Week43_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2022/wk42/HPS_Week42_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2022/wk41/HPS_Week41_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2021/wk40/HPS_Week40_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2021/wk39/HPS_Week39_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2021/wk38/HPS_Week38_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2021/wk37/HPS_Week37_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2021/wk36/HPS_Week36_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2021/wk35/HPS_Week35_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2021/wk34/HPS_Week34_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2021/wk33/HPS_Week33_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2021/wk32/HPS_Week32_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2021/wk31/HPS_Week31_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2021/wk30/HPS_Week30_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2021/wk29/HPS_Week29_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2021/wk28/HPS_Week28_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2021/wk27/HPS_Week27_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2021/wk26/HPS_Week26_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2021/wk25/HPS_Week25_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2021/wk24/HPS_Week24_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2021/wk23/HPS_Week23_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2021/wk22/HPS_Week22_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2020/wk21/HPS_Week21_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2020/wk20/HPS_Week20_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2020/wk19/HPS_Week19_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2020/wk18/HPS_Week18_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2020/wk17/HPS_Week17_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2020/wk16/HPS_Week16_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2020/wk15/HPS_Week15_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2020/wk14/HPS_Week14_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2020/wk13/HPS_Week13_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2020/wk12/HPS_Week12_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2020/wk12/pulse2020_puf_hhwgt_12.csv
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2020/wk11/HPS_Week11_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2020/wk11/pulse2020_puf_hhwgt_11.csv
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2020/wk10/HPS_Week10_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2020/wk10/pulse2020_puf_hhwgt_10.csv
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2020/wk9/HPS_Week09_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2020/wk9/pulse2020_puf_hhwgt_09.csv
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2020/wk8/HPS_Week08_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2020/wk8/pulse2020_puf_hhwgt_08.csv
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2020/wk7/HPS_Week07_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2020/wk7/pulse2020_puf_hhwgt_07.csv
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2020/wk6/HPS_Week06_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2020/wk5/pulse2020_puf_hhwgt_05.csv
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2020/wk4/HPS_Week04_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2020/wk4/pulse2020_puf_hhwgt_04.csv
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2020/wk3/HPS_Week03_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2020/wk2/HPS_Week02_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2020/wk2/pulse2020_puf_hhwgt_02.csv
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2020/wk1/HPS_Week01_PUF_CSV.zip
##        ◦ https://www2.census.gov/programs-surveys/demo/datasets/hhp/2020/wk1/pulse2020_puf_hhwgt_01.csv
##
##
#############################################################################################################################
#############################################################################################################################

# ==============================
# LOAD LIBRARIES
# ==============================

# ==============================
# SET FILE PATHS
# ==============================

# ==============================
# LOAD DATA
# ==============================

# ==============================
# PREPARE DATA
# ==============================

# ==============================
# CLEAN DATA
# ==============================

# ==============================
# SUBSET DATA FOR ANALYSIS
# ==============================

# ==============================
# TRANSGENDER ANALYSIS
# ==============================

# ==============================
# LGB ANALYSIS
# ==============================

# ==============================
# STATE-LEVEL AGGREGATE MAPS
# ==============================

