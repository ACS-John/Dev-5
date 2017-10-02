00010 ! Replace Test\console
00020 ! Sample Program demonstrating the FNConsole Feature
00030 ! ***********************************************************************
00040   library 'S:\Core\Library': fnconsole,fntop
00050 ! ***********************************************************************
00060   let fntop("Test\Console","Test Console")
00070 ! ***********************************************************************
00080   pr "I'm about to hide the console for 5 seconds"
00090   pr "press any key to begin"
00100   let kstat$(1)
00110   let fnconsole ! should turn console off
00111   let fnconsole ! should turn console off
00112   let fnconsole ! should turn console off
00113   let fnconsole ! should turn console off
00120 ! ***********************************************************************
00130   let sleep(5)
00140   pr newpage
00150   let fnconsole(1)
00151   let fnconsole(1)
00152   let fnconsole(1)
00160 ! ***********************************************************************
00170   pr 'how you like that?' !:
        pr 'Press any key to continue' !:
        let kstat$(1)
00180 ! Chain "Tease\Menu"
00185   stop 
