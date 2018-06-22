SELECT FirstName, LastName, JobTitle
FROM HumanResources.vEmployeeDepartment
ORDER BY 1

SELECT FirstName, LastName, JobTitle
FROM HumanResources.vEmployeeDepartment
ORDER BY 1, 2 DESC

SELECT FirstName, LastName, CountryRegionName
FROM Sales.vIndividualCustomer
ORDER BY CountryRegionName


SELECT FirstName, LastName, CountryRegionName
FROM Sales.vIndividualCustomer
WHERE CountryRegionName IN ('United States', 'France')
ORDER BY 3 


SELECT 
	Name,
	AnnualSales,
	YearOpened,
	SquareFeet AS [Store Size],
	NumberEmployees AS [Total Employees]
FROM Sales.vStoreWithDemographics
WHERE AnnualSales > 1000000 AND NumberEmployees >= 45
ORDER BY [Store Size] DESC, [Total Employees] DESC
