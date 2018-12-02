print("Hello py")

from urllib.request import urlopen
shakespeare = urlopen('http://www.baidu.com')
print(shakespeare.read().decode())