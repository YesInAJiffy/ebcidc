# Author: https://github.com/YesInAJiffy
# Contact: https://www.youtube.com/@NurtureLearning
# Date: August 13, 2023

def binary_to_ebcdic(binary_data):
    # EBCDIC encoding table
    ebcdic_encoding = {
        # Binary to EBCDIC mapping
        # ..
        0x00: b'\x00',  0x01: b'\x01',  0x02: b'\x02',  0x03: b'\x03',
        0x04: b'\x37',  0x05: b'\x2D',  0x06: b'\x2E',  0x07: b'\x2F',
        0x08: b'\x16',  0x09: b'\x05',  0x0A: b'\x25',  0x0B: b'\x0B'
    }
    ebcdic_values = [" ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ",
        " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ",
        " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ",
        " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ",
        " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", "¢", ".", "<", "(", " +", "|",
        "&", " ", " ", " ", " ", " ", " ", " ", " ", " ", "!", "$", "*", ")", " ;", "‥",
        "-", "/", " ", " ", " ", " ", " ", " ", " ", " ", " ", "‘", " ", "_", " >", "?",
        " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", ":", "#", "@", "'", "= ", "\"",
        " ", "a", "b", "c", "d", "e", "f", "g", "h", "i", " ", " ", " ", " ", " ", " ",
        " ", "j", "k", "l", "m", "n", "o", "p", "q", "r", " ", " ", " ", " ", " ", " ",
        " ", "~", "s", "t", "u", "v", "w", "x", "y", "z", " ", " ", " ", " ", " ", " ",
        " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ",
        " ", "A", "B", "C", "D", "E", "F", "G", "H", "I", " ", " ", " ", " ", " ", " ",
        " ", "J", "K", "L", "M", "N", "O", "P", "Q", "R", " ", " ", " ", " ", " ", " ",
        " ", " ", "S", "T", "U", "V", "W", "X", "Y", "Z", " ", " ", " ", " ", " ", " ",
        "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", " ", " ", " ", " ", " ", " " ]

    ebcdic_characters = []
    for byte in binary_data:
        ebcdic_characters.append(ebcdic_values[int(byte)])
        
    return ''.join(ebcdic_characters)

# Read binary data from a file
with open('EBCDIC_DATA.bin', 'rb') as binary_file:
    binary_data = binary_file.read()

# Convert binary data to EBCDIC
ebcdic_data = binary_to_ebcdic(binary_data)

# Write EBCDIC data to a file
with open('ebcdic_data.txt', 'w') as ebcdic_file:
    ebcdic_file.write(ebcdic_data)

