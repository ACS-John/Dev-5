10   library 's:\Core\Library': fnOpenFile,fnbuildkey$
11   dim statedata$(0)*128,statedatan(0),form$(0)*256
20   hstate=fnOpenFile('CO State',mat statedata$,mat statedatan,mat form$)
30   read #hstate,using form$(hstate): mat statedata$,mat statedatan
40 pr 'hstate=';hstate
50    pr 'build key returns "'&fnbuildkey$('CO State',mat statedata$,mat statedatan)&'"'
60 pr file$(hstate)
70 pause
