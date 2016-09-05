# coggle-expressions

Coggle Expression is an experimental lisp inspired language that generates coggle diagrams based on input code.
The project is currently in its infant stage, so everything experimental meaning things may not work as intended.  

### Usage  
From the terminal/commandline run the program with a text file containing your coggle expressions:
` ./expr.native coggle_code.txt
Replace the coggle_code.txt with your chosen text file.
From there follow the instructions that appear in your terminal shell.

#### Code
The coggle expression uses a tree like syntax with parent and children, which very similar to lisp.  

#### Create first Diagram
Branches are created by surrounding text in opening and closing parentheses e.g. ()

For example the following would generate a diagram with the name "Sports":
` ("Sports") ` 
The above would create a simple diagram with no branches.
**All forms of text must be surrounded in quotations.**  
To add a branch to above we must again use the parentheses ().
But this time we place it inside like so:  
` ("Sports" ("Football")) `  
So now the diagram will have a branch leading to football.

The first string of words will always be the diagram name. For example:  ` ("Favourite Fruits" ("Apple") ("Oranges") ("Grapes")) `
"Favourite Fruits " will be the name of the diagram.

#### Example
Copy and paste the code below into a text file and save as "music_diagram.txt":  
```
("music"
 ("hip hop" ("Nas") ("MF DOOM") ("Wu tang" ("GZA" "Ghostface")))
 ("Jazz" ("Coltrane") ("Miles Davis") ("Sonny Rollings"))
 ("Funk" ("James Brown") ("Parliment Funkadelic"))
 ("Pop" ("Janet jackson") ("Michael") ("Madonna")))
```
Now open up a terminal and run `./expr.native music_diagram.txt`  And follow the instructions.





### Binaries
Binaries are available for Mac OSX and Linux.  
Please contact me for download link.  