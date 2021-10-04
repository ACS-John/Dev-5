! Replace Test\console
! Sample Program demonstrating the FNConsole Feature
! ***********************************************************************
	autoLibrary
! ***********************************************************************
	fnTop("Test\Console","Test Console")
! ***********************************************************************
	pr "I'm about to hide the console for 5 seconds"
	pr "press any key to begin"
	kstat$(1)
	fnconsole ! should turn console off
	fnconsole ! should turn console off
	fnconsole ! should turn console off
	fnconsole ! should turn console off
! ***********************************************************************
	sleep(5)
	pr newpage
	fnconsole(1)
	fnconsole(1)
	fnconsole(1)
! ***********************************************************************
	pr 'how you like that?' : _
	pr 'Press any key to continue' : _
	kstat$(1)
! Chain "Tease\Menu"
	stop
