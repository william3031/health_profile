Please read this README.txt file before using the Screening_participation.csv file.
 
This README.txt accompanies the Screening_participation.csv file for the 'National cancer screening programs participation data' report, and contains data for participation for BreastScreen Australia, the National Bowel Cancer Screening Program (NBCSP) and the National Cervical Screening Program (NCSP). 

The file is intended for loading into analytical software for further analysis. For more information on the participation indicator, and how this is calculated, and geographic information please see Technical Notes.

Technical Notes: https://www.aihw.gov.au/reports/cancer-screening/national-cancer-screening-programs-participation/contents/technical-notes

The Screening_participation.csv file can be imported into a number of statistical software packages including SAS, R, or read by text editors including Notepad++. It contains the following variables:
 
Indicator: Performance indicator name
Program: Short name for the cancer screening program 
Sex: Sex (Males, Females, Persons)
Period: 2-year period over which participation are measured (1 year for the NCSP)
State: State or Territory, also includes Australia total.
Geography: Geographic area (i.e Total, PHN, SA3 or SA2). NCSP only has State or Territory data.
GeographicAreaName: Name of geographic area
GeographicCode: Code for geographic unit (i.e PHN101, or 11101, etc)
AgeGroup: Age group, includes 5-year age groups for the specific screening program target age range as available. 
MeasureName: Description of the measure displayed in the Value column 
ValueDescription: Description of data type displayed in the Value column
Value: Contains data values. Multiple data formats are included differentiated by the ValueDescription. Nulls in this field denote suppressed data that is not published because of small numbers, confidentiality or other concerns about the quality of the data
DataSource: Source dataset analyses are derived from
ReleaseName: AIHW Cat number for the report containing these data
ReleaseDate: Indicates the date the above report was released
URL: URL for the report page.

The data source for this report is AIHW analysis of BreastScreen Australia, NBCSP and NCSP data (from the relevant program data registers for the various years presented).

See Metadata_description_screening_participation.xlsx for more information.


