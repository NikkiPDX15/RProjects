SELECT * 
FROM Person.Person
WHERE MiddleName IS NULL

SELECT * 
FROM Person.Person
WHERE MiddleName IS NOT NULL

SELECT *
FROM HumanResources.vEmployee
WHERE MiddleName IS NOT NULL and PhoneNumberType = 'Cell'
