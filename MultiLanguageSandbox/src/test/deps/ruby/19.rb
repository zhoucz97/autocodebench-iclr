

# Sorts a list of patient information based on their age and registration order.
# The function takes an array of arrays, where each sub-array contains a patient's ID (a string) and age (an integer).
# The sorting criteria are as follows:
# 1. Patients aged 60 and above are given priority over younger patients.
# 2. Elderly patients (aged 60+) are sorted in descending order of age. If ages are equal, they are sorted by their registration order.
# 3. Younger patients (below 60) are sorted based on their registration order.
#
# Example:
# >>> sort_patients([["021075", 40], ["004003", 15], ["010158", 67], ["021033", 75], ["102012", 30]])
# [["021033", 75], ["010158", 67], ["021075", 40], ["004003", 15], ["102012", 30]]



def sort_patients(patient_info)
  # Separate elderly (60+) and younger patients
  elderly = patient_info.select { |id, age| age >= 60 }
  younger = patient_info.reject { |id, age| age >= 60 }
  
  # Sort elderly patients: descending by age, then by original order (index)
  elderly_sorted = elderly.sort_by { |patient| [-patient[1], patient_info.index(patient)] }
  
  # Sort younger patients: by original order (index)
  younger_sorted = younger.sort_by { |patient| patient_info.index(patient) }
  
  # Combine the two sorted lists
  elderly_sorted + younger_sorted
end

raise 'Test failed' unless sort_patients([["021075", 40], ["004003", 15], ["010158", 67], ["021033", 75], ["102012", 30]]) == [["021033", 75], ["010158", 67], ["021075", 40], ["004003", 15], ["102012", 30]]
raise 'Test failed' unless sort_patients([["001122", 60], ["003344", 60], ["005566", 30]]) == [["001122", 60], ["003344", 60], ["005566", 30]]
raise 'Test failed' unless sort_patients([["987654", 45], ["123456", 65], ["543210", 55]]) == [["123456", 65], ["987654", 45], ["543210", 55]]



puts 'All tests passed!'