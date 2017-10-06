00140   library 'S:\Core\Library': fnshortpath$
00160   dim pathToTest$*256
00180   setenv('acsDebugShortPath','Yes')
00300   pathToTest$="C:\Users\John\OneDrive\ACS\Dev-5D~1\Cerro Gordo\GLmstr\BudgetInfo.h5"
00340   pr fnshortpath$(pathToTest$)
00360   setenv('acsDebugShortPath','')

