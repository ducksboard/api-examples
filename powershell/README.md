PowerShell example
==================

Learn how to push data to Ducksboard using PowerShell


Requirements
---------------

You'll need:

* a Ducksboard account with API access
* Windows PowerShell 2.0

Usage
-----

	PS> Execute-DucksboardApi -url 'https://push.ducksboard.com/values/NUMERIC_ID/' -data '{"value": 0.56}' -apikey 'APIKEY'