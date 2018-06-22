SELECT Column1,...ColumnN
FROM SchemaName.TableName
WHERE ColumnName {Comparison Operator} [Filtering Criteria]

ORDER BY ColumnName, ColomOrdinal, ColumnAlias [ASC/DESC]



SELECT FirstName, LastName
FROM Sales.vIndividualCustomer
ORDER BY FirstName

SELECT FirstName, LastName
FROM Sales.vIndividualCustomer
ORDER BY 2

SELECT FirstName, LastName AS [Customer Last Name]
FROM Sales.vIndividualCustomer
ORDER BY [Customer Last Name] DESC


