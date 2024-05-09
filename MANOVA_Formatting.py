import pandas as pd
import numpy as np

'''We'd like to create a new dataframe that has evaluation (T1, T2, T3) as a column on the df, along with subject, and
have a column for each test conducted. This will make the set compatible with MANOVA'''


def write_columns(test_name, list_name, dataframe):
    for i in range(25):
        for j in range(1, 4):
            if test_name not in ["BBT", "GS"]:
                col_label = str(test_name) + f"_T{j}"

            else:
                col_label = str(test_name) + f"_T{j}_A"
            list_name.append(dataframe[col_label].loc[i])


df = pd.read_csv("toRotateBrainCanada.csv")

unique_participants = []
participants_col = []
follow_ups = []
unique_MEP_group = []
MEP_col = []
FMA_vals, BBT_vals, GS_vals = [], [], []
MAL_AOU_vals, MAL_QOU_vals = [], []
FMA_baseline_vals = []
BBT_baseline_vals = []
GS_baseline_vals = []
MAL_AOU_baseline_vals = []
MAL_QOU_baseline_vals = []
shoulder_baseline_vals = []
elbow_baseline_vals = []
wrist_baseline_vals = []
shoulder_AROM = []
elbow_AROM= []
wrist_AROM= []


tests = [["FMA", FMA_vals], ["BBT", BBT_vals], ["GS", GS_vals],
         ["MAL_Quant", MAL_AOU_vals], ["MAL_Quali", MAL_QOU_vals], ["ROM_Shoulder_Affected_Active", shoulder_AROM],
         ["ROM_Elbow_Affected_Active", elbow_AROM],["ROM_Wrist_Affected_Active", wrist_AROM]]
print(df)

# for i in range(25):
#     #unique_participants.append(df["Subject"][i])
#     #unique_MEP_group.append(df["MEP_Group"][i])
#     for j in range(1, 4):
#         participants_col.append(df.at["Subject"][i]) ##trying to append to list participants col
#         #used to be participants_col.append(df["Subject", i])
#         MEP_col.append(df["MEP_Group"][i])
#         follow_ups.append(f"T{j}")
#         FMA_baseline_vals.append(df["FMA_T1"][i])

for i in range(25):
    # Append participant ID for each follow-up
    for j in range(1, 4):
        participants_col.append(df.loc[i, 'Subject'])  # Corrected to use .loc[]
        MEP_col.append(df.loc[i, 'MEP_Group'])
        follow_ups.append(f'T{j}')
        FMA_baseline_vals.append(df.loc[i, 'FMA_T1'])
        BBT_baseline_vals.append(df.loc[i, 'BBT_T1_A'])
        GS_baseline_vals.append(df.loc[i, 'GS_T1_A'])
        MAL_AOU_baseline_vals.append(df.loc[i, 'MAL_Quant_T1'])
        MAL_QOU_baseline_vals.append(df.loc[i, 'MAL_Quali_T1'])
        shoulder_baseline_vals.append(df.loc[i, 'ROM_Shoulder_Affected_Active_T1'])
        elbow_baseline_vals.append(df.loc[i, 'ROM_Elbow_Affected_Active_T1'])
        wrist_baseline_vals.append(df.loc[i, 'ROM_Wrist_Affected_Active_T1'])


for test in tests:
    write_columns(test[0], test[1], df)

rotated_df = pd.DataFrame(list(zip(participants_col, follow_ups, MEP_col, FMA_vals, BBT_vals,
                                   GS_vals, MAL_AOU_vals, MAL_QOU_vals, shoulder_AROM, elbow_AROM, wrist_AROM,
                                   FMA_baseline_vals, BBT_baseline_vals, GS_baseline_vals, MAL_AOU_baseline_vals,
                                   MAL_QOU_baseline_vals, shoulder_baseline_vals, elbow_baseline_vals, wrist_baseline_vals)),
              columns=['Participant','Follow_Up','MEP_Group', 'FMA', "BBT", "Grip_Strength", "MAL_AOU", "MAL_QOU",
                       "Shoulder_AROM", "Elbow_AROM","Wrist_AROM", "FMA_baseline", "BBT_Baseline", "GS_Baseline",
                       "MAL_AOU_Baseline", "MAL_QOU_Baseline", "Shoulder_AROM_Baseline", "Elbow_AROM_baseline",
                       "Wrist_AROM_Baseline"])
print(rotated_df)
rotated_df.to_csv("BrainCanadaFinalRotated.csv")

#Add baseline FMA to covary against
