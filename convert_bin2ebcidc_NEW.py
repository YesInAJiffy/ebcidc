import struct

import csv

import chardet

values_list = []
with open('Mapping.csv', 'rb') as csvfile:
    result = chardet.detect(csvfile.read())
    encoding = result['encoding']
    with open('Mapping.csv', 'r', encoding=encoding) as csvfile:


        reader = csv.DictReader(csvfile)
        #dict_list = [{row['Decimal Value']: row['EBCDIC Symbol']} for row in reader]
        for row in reader:
            values_list.append(row['EBCDIC Symbol'])

#print(dict_list)
print(len(values_list))

#values_list = list(dict_list.values())
def binary_to_ebcdic(binary_data):
    ebcdic_string = ''
    for byte in binary_data:
        if values_list[byte] == "SP" :
            ebcdic_string += " "
        elif len(values_list[byte]) < 2:
            ebcdic_string += values_list[byte]
        else:
            ebcdic_string += ""
            #ebcdic_string += "{SPECIAL}"
    return ebcdic_string

# Example usage:
binary_data = b'Hello, World!'
#values_list = list(ebcdic_map.values())




print(binary_to_ebcdic(binary_data))

# Read binary data from a file
with open('BINFILE.DATA', 'rb') as binary_file:
    binary_data = binary_file.read()

# Convert binary data to EBCDIC
ebcdic_data = binary_to_ebcdic(binary_data)

# Write EBCDIC data to a file
with open('ebcdic_data_new.txt', 'w') as ebcdic_file:
    ebcdic_file.write(ebcdic_data)
