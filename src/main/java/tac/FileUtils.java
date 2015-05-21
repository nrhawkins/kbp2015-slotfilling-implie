package tac;

import java.io.File;
import java.io.FileInputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStream;
import java.io.RandomAccessFile;
import java.util.ArrayList;
import java.util.zip.GZIPInputStream;


public class FileUtils {

	/*
     * FilenameFilter where extension can be passed in rather than hardcoded
     */
    private static class FlexFilenameFilter implements FilenameFilter { 
        String ext; 

        public FlexFilenameFilter(String extension) { 
            if (extension != null){
                ext = extension; 
            } else {
                ext = "";
            }
        } 

        public boolean accept(File dir, String name) { 
            return name.endsWith(ext); 
        } 
    }

    public static FilenameFilter flexFilenameFilter(String extension){
        return new FlexFilenameFilter(extension);
    }
	
	/*
     * Find all files in a directory.
     */
    public static File[] findFiles(File inputDir) throws IOException {
       
        File[] files;
        if (inputDir.isDirectory()){
            files = inputDir.listFiles();
        } else {
            files = new File[1];
            files[0] = inputDir;
        }
        
        return files;
    }	
    
    /*
     * Find all files with extension in a directory and all sub-directories.
     */
    public static File[] findFiles(File inputDir, String extension) 
                                       throws IOException {
        return findFiles(inputDir, extension, false);
    }
	
	/*
     * Find all files with extension in a directory.
     * Optionally all sub-directories.
     */
    public static File[] findFiles(File inputDir, String extension, 
                                       boolean recursive) throws IOException {
        ArrayList<File> allFiles = new ArrayList<File>();
        FilenameFilter filter = flexFilenameFilter(extension);
        File[] files;
        if (inputDir.isDirectory()){
            files = inputDir.listFiles(filter);
        } else {
            files = new File[1];
            files[0] = inputDir;
        }

        if (recursive && files != null){
            for (File file : files){
                if (file.isDirectory()){
                    File[] moreFiles = findFiles(file, extension, true);
                    for (File f : moreFiles){
                        allFiles.add(f);
                    }
                } else {
                    allFiles.add(file);
                }
            }
        }
        return allFiles.toArray(new File[allFiles.size()]);
    }
	
    /*
     * Check whether input File is in GZIP format.
     * Return appropriate InputStream.
     */
    public static InputStream gzipCheck(File input) throws IOException{
        int magic = 0;
        try {
            RandomAccessFile raf = new RandomAccessFile(input, "r");
            magic = raf.read() & 0xff | ((raf.read() << 8) & 0xff00);
            raf.close();
        } catch (Throwable e) {
            e.printStackTrace(System.err);
        }
        InputStream inputStream = new FileInputStream(input);
        if (magic == GZIPInputStream.GZIP_MAGIC) {
            inputStream = new GZIPInputStream(inputStream);
        }
        return inputStream;
    }
    
    
}
