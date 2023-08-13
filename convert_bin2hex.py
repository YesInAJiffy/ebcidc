def binary_to_hex(input_filename, output_filename):
    with open(input_filename, 'rb') as binary_file:
        binary_data = binary_file.read()
        
    hex_data = ''.join(format(byte, '02X') for byte in binary_data)
    
    with open(output_filename, 'w') as hex_file:
        hex_file.write(hex_data)

input_file = 'EBCDIC_DATA.bin'
output_file = 'hex_output.txt'
binary_to_hex(input_file, output_file)

