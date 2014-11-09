
## source

[portraitCantonauxOFS](http://www.bfs.admin.ch/bfs/portal/fr/index/regionen/kantone/daten.Document.68999.xls)

* [pageGeneral](http://www.bfs.admin.ch/bfs/portal/fr/index/regionen/kantone/daten.html)

## OpenRefine

1. Create new projet
    * Ignore first 3 lines
    * Do not store blank rows
    * Load at most 75 rows of data
1. Clean
    * Années *fill down*
	* Remove comments "1)" or "2)" : *value.replace(/(\d+\))/, " ")*


    * Create new column "type"

```
if(isNull(value.match(/Population|Langue principale en .*|Appartenance à une religion en .*|Surface en km2|Mobilité|Hôpitaux|Protection sociale|Niveau de formation .*|Partis politiques en .*|Travail|Santé|Economie|Mobilité|Logements|Langue principale en .*/)), "", value) 
```  

   * Fill down
   * Delete the rows used to define type
      * Select a data colum
      * Filter by blank values
      * Delete all matching rows
1. Export as tsv
      
      
#### Open Refine JSON Code
      
      
```
[
  {
    "op": "core/text-transform",
    "description": "Text transform on cells in column Column using expression grel:value.replace(/(\\d+\\))/, \" \")",
    "engineConfig": {
      "facets": [],
      "mode": "row-based"
    },
    "columnName": "Column",
    "expression": "grel:value.replace(/(\\d+\\))/, \" \")",
    "onError": "keep-original",
    "repeat": false,
    "repeatCount": 10
  },
  {
    "op": "core/fill-down",
    "description": "Fill down cells in column Années",
    "engineConfig": {
      "facets": [],
      "mode": "row-based"
    },
    "columnName": "Années"
  },
  {
    "op": "core/column-addition",
    "description": "Create column type at index 1 based on column Column using expression grel:if(isNull(value.match(/Population|Langue principale en .*|Appartenance à une religion en .*|Surface en km2|Mobilité|Hôpitaux|Protection sociale|Niveau de formation .*|Partis politiques en .*|Travail|Santé|Economie|Mobilité|Logements|Langue principale en .*/)), \"\", value)",
    "engineConfig": {
      "facets": [],
      "mode": "row-based"
    },
    "newColumnName": "type",
    "columnInsertIndex": 1,
    "baseColumnName": "Column",
    "expression": "grel:if(isNull(value.match(/Population|Langue principale en .*|Appartenance à une religion en .*|Surface en km2|Mobilité|Hôpitaux|Protection sociale|Niveau de formation .*|Partis politiques en .*|Travail|Santé|Economie|Mobilité|Logements|Langue principale en .*/)), \"\", value)",
    "onError": "set-to-blank"
  },
  {
    "op": "core/fill-down",
    "description": "Fill down cells in column type",
    "engineConfig": {
      "facets": [],
      "mode": "row-based"
    },
    "columnName": "type"
  },
  {
    "op": "core/row-removal",
    "description": "Remove rows",
    "engineConfig": {
      "facets": [
        {
          "to": 640000,
          "expression": "value",
          "selectError": true,
          "selectNumeric": false,
          "selectBlank": true,
          "name": "Suisse",
          "columnName": "Suisse",
          "selectNonNumeric": true,
          "from": 0,
          "type": "range"
        }
      ],
      "mode": "row-based"
    }
  }
]
```