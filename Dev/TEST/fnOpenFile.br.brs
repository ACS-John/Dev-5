autoLibrary
dim statedata$(0)*128,statedatan(0),form$(0)*256
hstate=fnOpenFile('CO State',mat statedata$,mat statedatan,mat form$)
read #hstate,using form$(hstate): mat statedata$,mat statedatan
pr 'hstate=';hstate
pr 'build key returns "'&fnbuildkey$('CO State',mat statedata$,mat statedatan)&'"'
pr file$(hstate)
pause
