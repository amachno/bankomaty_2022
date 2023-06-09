import pandas as pd
import matplotlib.pyplot as plt

def filter_and_remove(df, name):
    # filter the dataframe based on the "name" column matching the specified value
    filtered_df = df[df['name'] == name].copy()

    # remove the "name" column from the filtered dataframe
    filtered_df = filtered_df.drop('name', axis=1)

    return filtered_df

def ends_with_number(column_name, numbers):
    for number in numbers:
        if column_name.endswith(str(number)):
            return True
    return False

def change_id(df):
    # create a new dataframe with the same data as the input dataframe
    new_df = df.copy()

    # create a dictionary to map periodIDs to integer values
    period_dict = {period: i for i, period in enumerate(sorted(new_df['periodID'].unique()))}

    # map the periodIDs to integer values and assign them to the "periodID" column
    new_df['periodID'] = new_df['periodID'].map(period_dict) + 1

    return new_df

def summary_statistics_summary(data):
    summary_stats = data.describe().transpose()
    summary_stats_summary = summary_stats.describe()
    
    # Rename columns and index
    summary_stats_summary.columns = ['Count', 'Mean', 'Standard Deviation', 'Minimum', '25th Percentile', 'Median', '75th Percentile', 'Maximum']
    summary_stats_summary.index = ['Statistic Count', 'Statistic Mean', 'Statistic Standard Deviation', 'Statistic Minimum', 'Statistic 25th Percentile', 'Statistic Median', 'Statistic 75th Percentile', 'Statistic Maximum']
    
    # Round values and remove decimals
    summary_stats_summary = summary_stats_summary.round(0).astype(int)
    
    return summary_stats_summary

