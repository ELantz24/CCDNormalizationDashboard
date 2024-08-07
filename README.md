# CCDNormalizationDashboard


This dashboard is to help anyone understand healthcare data quickly and in a visual format. It can help inform analysts and developers on the normalization efforts in the data pipeline. There are 8 datasets: vitals, unmappedVitals, demo, social, meds, labs, unmappedLabs, and problems. The file in this repository has a spot to upload the code. This will be indicated with the comment #DUMMY DATA. 

Vitals/UnmappedVitals Variables
Patient ID: char
Code: char
Code.Set: char
Name: char
Value: num
Unit: char
Date: Date
State: char
Normalized.Name: char

Demo Variables
PatientID: char
Given.Name: char
Family.Name: char
DOB: Date
Gender: char
Phone.Number: char
Line.Address: char
City.Address: char
State.Address: char
Postal.Code.Address: char
Country.Address: chr
Religious.Affiliation: char

Social Variables
Patient ID: char
Code: char
Code.Set: char
Name: char
Value.Code: char
Value.Code.Set: char
Value.Description: char
Start.Date: Date
End.Date: Date
Normalized.Name: char



Meds Variables
PatientID: char
Name: char
Start.Date: char
Dosage.Value: num
Dosage.Unit: char
Indication.Description: char
Instructions: char
Code.Set: cjar
Normalized.Name: char

Labs/UnmappedLabs Variables
PatientID: char
Code: char
Code.Set: char
Name: char
Value: num
Unit: char
Date: Date
Status: char
Interpration.Code: char
Intrepration.Name: char
Normalized.Name: char

Problems Variables
PatientID: char
Code: char
Name: char
Code.Set: char
State.Date: Date
End.Date: Date
Status: char
Normalized.Name: char
