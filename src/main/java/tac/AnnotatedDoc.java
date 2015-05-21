package tac;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.Date;

public class AnnotatedDoc {
	private Date docDate;
	private String docDateString;
    private String docID;
    private ArrayList<Sentence> sentences;

    /*
     * Empty constructor exists mostly to allow for use of Jackson ObjectMapper
     * to de/serialize in JSON. Should probably not be used otherwise.
     * @see Utils#deserilaizeAnnotatedDoc(File)
     * @see Utils#deserilaizeAnnotatedDocs(File)
     */
    public AnnotatedDoc(){
        docID = "";
        sentences = new ArrayList<Sentence>();
    }

    public AnnotatedDoc(String id){
        docID = new String(id);
        sentences = new ArrayList<Sentence>();
    }

    public AnnotatedDoc(String id, String dateString, Date date){
        docID = new String(id);
        docDateString = new String(dateString);
        docDate = date;        
        sentences = new ArrayList<Sentence>();
    }
    
    public void setID(String id){
        docID = new String(id);
    }
    
    public void setDateString(String dateString){
        docDateString = new String(dateString);
    }

    public void setDate(Date date){
        docDate = date;
    }
    
    public void addSentence(String sentence, int startSpan, int endSpan){
        Sentence s = new Sentence(sentence, startSpan, endSpan);
        sentences.add(s);
    }
    
    public void addSentence(Sentence sentence){
        sentences.add(new Sentence(sentence));
    }

    public String getID(){
        return docID;
    }
    
    public String getDateString(){
        return docDateString;
    }
    
    public Date getDate(){
        return docDate;
    }    
    
    public ArrayList<Sentence> getSentences(){
        return sentences;
    }
    
    public String getFirstSentence(){
        return sentences.get(0).getSentence();
    }

    public int size(){
        return sentences.size();
    }

    public Iterator<Sentence> iterator(){
        return sentences.iterator();
    } 
}


