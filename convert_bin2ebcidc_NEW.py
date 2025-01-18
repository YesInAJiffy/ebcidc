import struct

import csv

import chardet

values_list = []
ignoreSpecial = True
with open('Mapping.csv', 'rb') as csvfile:
    result = chardet.detect(csvfile.read())
    encoding = result['encoding']
    with open('Mapping.csv', 'r', encoding=encoding) as csvfile:
        reader = csv.DictReader(csvfile)
        for row in reader:
            values_list.append(row['EBCDIC Symbol'])

print(len(values_list))

def binary_to_ebcdic(binary_data):
    ebcdic_string = ''
    for byte in binary_data:
        if values_list[byte] == "SP":# or values_list[byte] == "NUL":
            ebcdic_string += " "
        elif len(values_list[byte]) < 2:
            ebcdic_string += values_list[byte]
        #elif values_list[byte] == "NL" or values_list[byte] == "LF" or values_list[byte] == "RNL":
        #elif values_list[byte] == "SOH":
        #    ebcdic_string += "\n"
        elif ignoreSpecial:
            ebcdic_string += ""
        else:
            adval = "{{" + values_list[byte]+ "}}"
            ebcdic_string += adval
    return ebcdic_string




# Read binary data from a file
with open('CDMAST.DATA', 'rb') as binary_file:
    binary_data = binary_file.read()

# Convert binary data to EBCDIC
ebcdic_data = binary_to_ebcdic(binary_data)

# Write EBCDIC data to a file
with open('ebcdic_data_new.txt', 'w') as ebcdic_file:
    ebcdic_file.write(ebcdic_data)
