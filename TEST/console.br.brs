00010 ! Replace Test\console
00020 ! Sample Program demonstrating the FNConsole Feature
00030 ! ***********************************************************************
00040   library 'S:\Core\Library': fnconsole,fntop
00050 ! ***********************************************************************
00060   fntop("Test\Console","Test Console")
00070 ! ***********************************************************************
00080   pr "I'm about to hide the console for 5 seconds"
00090   pr "press any key to begin"
00100   kstat$(1)
00110   fnconsole ! should turn console off
00111   fnconsole ! should turn console off
00112   fnconsole ! should turn console off
00113   fnconsole ! should turn console off
00120 ! ***********************************************************************
00130   sleep(5)
00140   pr newpage
00150   fnconsole(1)
00151   fnconsole(1)
00152   fnconsole(1)
00160 ! ***********************************************************************
00170   pr 'how you like that?' !:
        pr 'Press any key to continue' !:
        kstat$(1)
00180 ! Chain "Tease\Menu"
00185   stop 
