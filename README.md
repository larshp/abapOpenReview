# abapOpenReview
ABAP Review Tool

## Design goals

* Work via SAP GUI(ie. no Eclipse integration)
* Version requirement: 702
* Customizable and extendable
* Install via [abapGit](https://github.com/larshp/abapGit)

## Integration in the transport system
abapOpenReview comes with two transport system enhancements:
* When a transport request contains code diff, it must be approved before the transport request can be released. This check can be deactivated.
* Reviews are closed after a transport request is released.

## Default checklist
The repository contains the program `zaor_import_default_checklist` to import a default checklist with the following items:
- `Dependencies changed in other transport requests?` When yes, the code will proparly break in target system, when this transport request is transported before the transport requests with the dependencies.
- `All associated Customizing-Settings contained in transport request or are already created manually /via ALE-transfer in target system (e.g. Characteristics in CT04, Classes in CL02)?`: Not all Customizing-Settings are transported automatically e.g. the characteristics in transaction CT04, the classes in transaction CL02 (both from the classification API) or z-tables not marked correctly as customizing-tables. In this point we should make sure, that the Customizing-Settings between source and target system don't differ.
- `API-Major-Changes (class, function-module etc.) bumped to dependents?`: When your transport request introduces major-changes, the dependents, which uses the APIs, can break. In this point we should make sure, that the dependents are compatible with the new API.
