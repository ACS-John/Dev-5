! r: doNotInclude
	pr 'This clip is not intended to be compiled directly nor run directly.'
	pr 'This clip replaces "include: fn_setup" when processed.'

	pr 'This area inside the doNotInclude region may be used for testing or documentation.'

	end
! /r doNotInclude

def fn_setup
	if ~setup then
		setup=1
		autoLibrary
		gosub Enum
		on error goto Ertn
	end if
fn

include: Enum
include: Ertn
