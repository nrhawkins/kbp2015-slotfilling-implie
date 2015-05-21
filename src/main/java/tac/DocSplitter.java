package tac;

import java.io.BufferedReader;
import java.io.File;
//import java.io.FileReader;
//import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
//import java.io.OutputStream;
import java.util.ArrayList;
import java.util.regex.Pattern;
//import java.util.regex.Matcher;
import java.text.SimpleDateFormat;
import java.util.Date;
//import edu.washington.multirframework.annotation.AnnotatedDoc;

public class DocSplitter {

	public ArrayList<AnnotatedDoc> convert(File input, boolean replaceXML) throws IOException {
		
		InputStream inputStream = FileUtils.gzipCheck(input);
		InputStreamReader inputStreamReader = new InputStreamReader(inputStream, "UTF8");
		BufferedReader reader = new BufferedReader(inputStreamReader);
		
        ArrayList<AnnotatedDoc> allDocs = new ArrayList<AnnotatedDoc>();
        Pattern docIdPattern = Pattern.compile("^<DOC\\s+id=.*", Pattern.CASE_INSENSITIVE);
        SimpleDateFormat ft = new SimpleDateFormat("yyyyMMdd");
        Pattern docClosePattern = Pattern.compile("\\s*(</DOC>|</doc>)\\s*");       
        //String with 100 spaces - use this to take subsets of spaces of length < 100 spaces
        String emptyString100 = "                                                                                                    ";
        String line = reader.readLine();
        
        while ((line != null)){
        	
            boolean done = false;
            String docid = "";
            String docDateString = "";
            Date docDate = new Date(); 
            int startOffset = 0;
            StringBuilder contents = new StringBuilder();        	
            contents.setLength(0);
            
        	while(!done){
                
        		if (docClosePattern.matcher(line).matches()) done = true;                        		

        		if (docIdPattern.matcher(line).find()){
        			
        			docid = line.substring(9).split("\"")[0];        		
        			
        			try{
        			docDateString = docid.split("\\.")[0];
        			}catch(Exception e){
        				System.err.println("Couldn't split doc id on a period.");
        			}        			
        			try{
                    String[] splitString = docDateString.split("_");
                    docDateString = splitString[splitString.length - 1];
        			} catch(Exception e){
        				System.err.println("Couldn't split doc id on underscore.");
        			}
                    try{
                       docDate = ft.parse(docDateString);
                    }catch(Exception e) {
                       System.err.println("Couldn't parse doc id date: " + docid);	
                    }
        			
        			if(replaceXML){
        			   contents.append(emptyString100.substring(0,line.length()));
        			}
        			else{
        			   //in English docs, need this, else text at end of one line and 
        			   //beginning of next line are adjacent
        		       contents.append(line + " ");
        			}
        		}        		
        		else{                
        		   //in Gigaword English docs, need this, else text at end of one line and 
     			   //beginning of next line are adjacent	
                   contents.append(line + " ");
        		}
                
                line = reader.readLine();               
           }
        	
           AnnotatedDoc doc = new AnnotatedDoc(docid, docDateString, docDate); 
           
           if(replaceXML){
        	   
        	   String cleanString = new String();
        	   cleanString = contents.toString();
        	   //Replace the 10-char tag, <HEADLINE>, with 10 spaces
               cleanString = cleanString.replace("<HEADLINE>", "          ");
               //Replace the 11-char tag, </HEADLINE>, with a period (.) plus 10 spaces
               //The period will allow the headline to be split as a sentence by Stanford CoreNLP ssplit
               cleanString = cleanString.replace("</HEADLINE>", ".          ");        
               //Replace the 10-char tag, <DATELINE>, with 10 spaces
               cleanString = cleanString.replace("<DATELINE>", "          ");       
               //Replace the 11-char tag, </DATELINE>, with a period (.) plus 10 spaces               
               cleanString = cleanString.replace("</DATELINE>", ".          ");
               //Replace the 3-char tag, <P>, with 3 spaces
               cleanString = cleanString.replace("<P>", "   ");  
               //Replace the 4-char tag, </P>, with 4 spaces
               cleanString = cleanString.replace("</P>", "    ");
               //Replace the 6-char tag, <TEXT>, with 6 spaces
               cleanString = cleanString.replace("<TEXT>", "      ");
               //Replace the 7-char tag, </TEXT>, with 7 spaces
               cleanString = cleanString.replace("</TEXT>", "       ");
               //Replace the 6-char tag, </DOC>, with 6 spaces
               cleanString = cleanString.replace("</DOC>", "      "); 	   

               //"Sentence" being added is a string of the entire document, 
               //where the <tags> have been replaced by spaces
               doc.addSentence(cleanString, startOffset, startOffset+cleanString.length()); 
               
           }
           else{
        	  //"Sentence" being added is a string of the entire document, 
              doc.addSentence(contents.toString(), startOffset, startOffset+contents.length());       
           }
                            
           
           allDocs.add(doc);           
                
        }
        reader.close();

        return allDocs;
    }

    
}

