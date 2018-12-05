print(3 ** 4) # 乘方
print(13 //3 ) #整除

shoplist = ["apple", "mango", "carrot", "banana"]
print('item 1 to -1 is', shoplist[1:-1])
print(shoplist[::2])
print(shoplist[::-1])

import os
print('os.sep ' + os.sep)

#print(help(int))

class Person:

    classMember = 0

    def __init__(self, name):
        self.name = name
        Person.classMember += 1

    def say_hi(self):
        print('say_hi', self.name)

    @classmethod
    def how_many(cls):
        print('classnumber', cls.classMember)


class Teacher(Person):
    def __init__(self, name, age):
        Person.__init__(self, name)
        self.age = age;

p = Person("gaga")
p.say_hi()
p2 = Person("tata")
p2.say_hi()
print(Person.classMember)
Person.how_many()

p3 = Teacher("xx", 31)
p3.say_hi()