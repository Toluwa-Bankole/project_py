FIRST = "Tolu"
LAST = "Bankole"
FULL = FIRST + " " + LAST  # CONCATENATION APPROACH
print(FULL)

# formatting strings approach
middle = "James"

new = f"{FIRST} {LAST} {middle}"
# any inbuilt function can be used in the f string, also an expression can be used in the f string, for example 2+3 can be used in the f string and it will evaluate to 5 and be printed in the output
new2 = f"{len(FIRST)} {LAST} {middle}"
new3 = f"{len(FIRST) + 5} {LAST} {middle}"
new3 = f"{len(FIRST) + 5} {2+3} {middle}"
new4 = f"x: {len(FIRST) + 5}, y: {2+3} {middle}"

print(new)
print(new2)
print(new3)
print(new4)


# changing the case of the string
print(FULL.upper())  # this will change the string to uppercase
print(FULL.lower())  # this will change the string to lowercase
print(FULL.title())  # this will change the string to title case, which means the first letter of each word will be capitalized and the rest will be lowercase


# removing whitespace from the string
FIRST_n = "  Tolu"
LAST_n = "Bankole    "
FULL_n = FIRST_n + "   " + LAST_n
# this will remove any whitespace from the beginning and end of the string
print(FULL_n.strip())
# this will remove any whitespace from the beginning of the string
print(FULL_n.lstrip())
# this will remove any whitespace from the end of the string
print(FULL_n.rstrip())
# this will remove any whitespace from the string and join the words with a single space in between
print(" ".join(FULL_n.split()))

# finding a substring in a string
# this will return the index of the first occurrence of the substring "Bankole" in the string FULL, which is 6
print(FULL.find("Bankole"))
# this will return -1 because the substring "James" is not found in the string FULL
print(FULL.find("James"))
# this will return the index of the first occurrence of the substring "ole" in the string FULL, which is 9
print(FULL.find("ole"))

# replacing a substring in a string
# this will replace the substring "Bank" with "M" in the string FULL
print(FULL.replace("Bank", "M"))

# check for existence of a character in a string value
# this will return Boolean True because the substring "Bank" is found in the string FULL
print("Bank" in FULL)
# this will return Boolean False because the substring "James" is not found in the string FULL
print("Games" not in FULL)
