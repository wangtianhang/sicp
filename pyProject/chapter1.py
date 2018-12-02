from urllib.request import urlopen
shakespeare = urlopen('file:///E:/Dev/GitHub_Self/sicp/sicp/pyProject/shakespeare.txt')
print(shakespeare.read().decode())