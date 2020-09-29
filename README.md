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
