SELECT FirstName, LastName
FROM Sales.vIndividualCustomer
ORDER BY 2 DESC

SELECT FirstName, LastName
FROM Sales.vIndividualCustomer
ORDER BY LastName ASC, FirstName DESC

SELECT 
	FirstName AS [Customer First Name], LastName
FROM Sales.vIndividualCustomer
ORDER BY 2, [Customer First Name]

SELECT LastName, FirstName, SalesQuota
FROM Sales.vSalesPerson
WHERE SalesQuota >= 250000
ORDER BY SalesQuota DESC, LastName ASC



