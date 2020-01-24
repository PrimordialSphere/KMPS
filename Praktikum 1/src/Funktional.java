import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;

public class Funktional {
	static ArrayList<String> token_list = new ArrayList<String>();
	static ArrayList<Album> albums = new ArrayList<Album>();
	static final Boolean is_in_album = false;
	static final Boolean is_in_track = false;
	static final  int current_album = 0;
	static final int current_track = 0;
	static final int current_character = 0;
	static String home = System.getProperty("user.home");
	static String file_path = (home
			+ "/eclipse-workspace/KMP-Praktikum1/src/data/alben.xml");

public static int findOppeningTag (byte [] file_contents, int current_character) {
	return findOppeningTagHelper(file_contents, current_character, 0);
}
public static int findOppeningTagHelper (byte [] file_contents, int current_character, int title_length) {
	if(file_contents[current_character + title_length] != '<') {
		return findOppeningTagHelper(file_contents, current_character, title_length+1);
	}
	return title_length;
}

	public static ArrayList<String> createTokenListHelper(byte[] file_contents,
			ArrayList<String> token_list, int current_character) {
		if (current_character < file_contents.length){
		if(file_contents[current_character] == '\n' || file_contents[current_character] == '\r' || file_contents[current_character] == '\t'){
			return createTokenListHelper(file_contents, token_list, current_character+1);
		}
		else if(new String(file_contents, current_character, 7, StandardCharsets.UTF_8).equals(new String("<album>"))){
			token_list.add(new String(file_contents, current_character+1, 5,
					StandardCharsets.UTF_8));
			return createTokenListHelper(file_contents, token_list, current_character+7);
			

		}
		else if(new String(file_contents, current_character, 8, StandardCharsets.UTF_8).equals(new String("</album>"))){
			token_list.add(new String(file_contents, current_character+1, 6,StandardCharsets.UTF_8));
			return createTokenListHelper(file_contents, token_list, current_character+8);
		}
		else if(new String(file_contents, current_character, 7, StandardCharsets.UTF_8).equals(new String("<title>"))){
			token_list.add(new String(file_contents, current_character+1, 5,StandardCharsets.UTF_8));
			token_list.add(new String(file_contents, current_character+7, findOppeningTag(file_contents, current_character+7),StandardCharsets.UTF_8));
			return createTokenListHelper(file_contents, token_list, current_character+7);
	

		}
		else if(new String(file_contents, current_character, 8, StandardCharsets.UTF_8).equals(new String("</title>"))){
			token_list.add(new String(file_contents, current_character+1, 6,StandardCharsets.UTF_8));
			return createTokenListHelper(file_contents, token_list, current_character+7);
		

		}
		else if(new String(file_contents, current_character, 8, StandardCharsets.UTF_8).equals(new String("<artist>"))){
			token_list.add(new String(file_contents, current_character+1, 6,StandardCharsets.UTF_8));
			token_list.add(new String(file_contents, current_character+8, findOppeningTag(file_contents, current_character+8),StandardCharsets.UTF_8));
			return createTokenListHelper(file_contents, token_list, current_character+8);
	
		}
		else if(new String(file_contents, current_character, 9, StandardCharsets.UTF_8).equals(new String("</artist>"))){
			token_list.add(new String(file_contents, current_character+1, 7,StandardCharsets.UTF_8));
			return createTokenListHelper(file_contents, token_list, current_character+9);
		
		}
		else if(new String(file_contents, current_character, 8, StandardCharsets.UTF_8).equals(new String("<rating>"))){
			token_list.add(new String(file_contents, current_character+1, 6,StandardCharsets.UTF_8));
			token_list.add(new String(file_contents, current_character+8, findOppeningTag(file_contents, current_character+8),StandardCharsets.UTF_8));
			return createTokenListHelper(file_contents, token_list, current_character+8);
		
		}
		else if(new String(file_contents, current_character, 9, StandardCharsets.UTF_8).equals(new String("</rating>"))){
			token_list.add(new String(file_contents, current_character+1, 7,StandardCharsets.UTF_8));
			return createTokenListHelper(file_contents, token_list, current_character+9);
		
		}
		else if(new String(file_contents, current_character, 7, StandardCharsets.UTF_8).equals(new String("<track>"))){
			token_list.add(new String(file_contents, current_character+1, 5,StandardCharsets.UTF_8));
			return createTokenListHelper(file_contents, token_list, current_character+7);
			
		}
		else if(new String(file_contents, current_character, 8, StandardCharsets.UTF_8).equals(new String("</track>"))){
			token_list.add(new String(file_contents, current_character+1, 6,StandardCharsets.UTF_8));
			return createTokenListHelper(file_contents, token_list, current_character+8);
		
		}
		else if(new String(file_contents, current_character, 9, StandardCharsets.UTF_8).equals(new String("<feature>"))){
			token_list.add(new String(file_contents, current_character+1, 7,StandardCharsets.UTF_8));
			token_list.add(new String(file_contents, current_character+9, findOppeningTag(file_contents, current_character+9),StandardCharsets.UTF_8));
			return createTokenListHelper(file_contents, token_list, current_character+9);
			
		}
		else if(new String(file_contents, current_character, 10, StandardCharsets.UTF_8).equals(new String("</feature>"))){
			token_list.add(new String(file_contents, current_character+1, 8,StandardCharsets.UTF_8));
			return createTokenListHelper(file_contents, token_list, current_character+10);
			
		}
		else if(new String(file_contents, current_character, 8, StandardCharsets.UTF_8).equals(new String("<length>"))){
			token_list.add(new String(file_contents, current_character+1, 6,StandardCharsets.UTF_8));
			token_list.add(new String(file_contents, current_character+8, findOppeningTag(file_contents, current_character+8),StandardCharsets.UTF_8));
			return createTokenListHelper(file_contents, token_list, current_character+8);
			
		}
		else if(new String(file_contents, current_character, 9, StandardCharsets.UTF_8).equals(new String("</length>"))){
			token_list.add(new String(file_contents, current_character+1, 7,StandardCharsets.UTF_8));
			return createTokenListHelper(file_contents, token_list, current_character+9);
		
		}
		else if(new String(file_contents, current_character, 9, StandardCharsets.UTF_8).equals(new String("<writing>"))){
			token_list.add(new String(file_contents, current_character+1, 7,StandardCharsets.UTF_8));
			token_list.add(new String(file_contents, current_character+9, findOppeningTag(file_contents, current_character+9),StandardCharsets.UTF_8));
			return createTokenListHelper(file_contents, token_list, current_character+9);
		
		}
		else if(new String(file_contents, current_character, 10, StandardCharsets.UTF_8).equals(new String("</writing>"))){
			token_list.add(new String(file_contents, current_character+1, 8,StandardCharsets.UTF_8));
			return createTokenListHelper(file_contents, token_list, current_character+10);
			
		}
		else if(new String(file_contents, current_character, 6, StandardCharsets.UTF_8).equals(new String("<date>"))){
			token_list.add(new String(file_contents, current_character+1, 4,StandardCharsets.UTF_8));
			token_list.add(new String(file_contents, current_character+6, findOppeningTag(file_contents, current_character+6),StandardCharsets.UTF_8));
			return createTokenListHelper(file_contents, token_list, current_character+6);
			
		}
		else if(new String(file_contents, current_character, 7, StandardCharsets.UTF_8).equals(new String("</date>"))){
			token_list.add(new String(file_contents, current_character+1, 5,StandardCharsets.UTF_8));
			return createTokenListHelper(file_contents, token_list, current_character+7);
		
		}
		else {			
		return createTokenListHelper(file_contents, token_list, current_character+1);
		
		}
		}
		return token_list;
	}

	public static ArrayList<String> createTokenList(byte[] file_contents, ArrayList<String> token_list) {
		return createTokenListHelper(file_contents, token_list, 0);
	}

	public static ArrayList<Album> parseFileHelper (ArrayList<String> token_list, ArrayList<Album> albums, Boolean is_in_album, Boolean is_in_track, int current_album, int current_track, int current_character, int counter)  {
		
		if(counter < token_list.size()) {
			
		if(token_list.get(counter).equals("album")){
			albums.add(new Album());
			return parseFileHelper (token_list, albums, true, is_in_track, albums.size()-1, current_track, current_character, counter+1);
		}
		else if(token_list.get(counter).equals("/album")){
			return parseFileHelper (token_list, albums, false, is_in_track, current_album, current_track, current_character, counter+1);
		}
		else if(token_list.get(counter).equals("title")){
			if(is_in_track) {
				albums.get(current_album).tracks.get(current_track).title = token_list.get(counter+1);}
			else if(is_in_album) {
				albums.get(current_album).title = token_list.get(counter+1);}
			return parseFileHelper (token_list, albums, is_in_album, is_in_track, current_album, current_track, current_character, counter+1);
		}
		else if(token_list.get(counter).equals("artist")){
			albums.get(current_album).artist = token_list.get(counter+1);
			return parseFileHelper (token_list, albums, is_in_album, is_in_track, current_album, current_track, current_character, counter+1);
		}
		else if(token_list.get(counter).equals("rating")){
			albums.get(current_album).tracks.get(current_track).rating = Integer.parseInt(token_list.get(counter+1));
			return parseFileHelper (token_list, albums, is_in_album, is_in_track, current_album, current_track, current_character, counter+1);
		}
		else if(token_list.get(counter).equals("track")){
			albums.get(current_album).tracks.add(new Track());
	
			return parseFileHelper (token_list, albums, is_in_album, true, current_album, albums.get(current_album).tracks.size()-1, current_character, counter+1);
		}
		else if(token_list.get(counter).equals("/track")){
			return parseFileHelper (token_list, albums, is_in_album, false, current_album, current_track, current_character, counter+1);
		}
		else if(token_list.get(counter).equals("feature")){
			albums.get(current_album).tracks.get(current_track).features.add(token_list.get(counter+1));
			return parseFileHelper (token_list, albums, is_in_album, is_in_track, current_album, current_track, current_character, counter+1);
		}
		else if(token_list.get(counter).equals("length")){
			albums.get(current_album).tracks.get(current_track).length = token_list.get(counter+1);
			return parseFileHelper (token_list, albums, is_in_album, is_in_track, current_album, current_track, current_character, counter+1);
		}
		else if(token_list.get(counter).equals("writing")){
			albums.get(current_album).tracks.get(current_track).writers.add(token_list.get(counter+1));
			return parseFileHelper (token_list, albums, is_in_album, is_in_track, current_album, current_track, current_character, counter+1);
		}
		else if(token_list.get(counter).equals("date")){
			albums.get(current_album).date = token_list.get(counter+1);
			return parseFileHelper (token_list, albums, is_in_album, is_in_track, current_album, current_track, current_character, counter+1);
		}
		else {
			return parseFileHelper (token_list, albums, is_in_album, is_in_track, current_album, current_track, current_character, counter+1);
		}
		
		}
		
		return albums;
		
		}	
	
public static ArrayList<Album> parseFile (ArrayList<String> token_list, ArrayList<Album> albums, Boolean is_in_album, Boolean is_in_track, int current_album, int current_track, int current_character)  {
		
	
	return parseFileHelper(token_list, albums, is_in_album, is_in_track, current_album, current_track, current_character, 0);
	}
	

	
	
	public static void main(String[] args) throws IOException {

		
		byte[] file_contents = Files.readAllBytes(Paths.get(file_path));
		
		token_list = createTokenList(file_contents, token_list);
		albums =  parseFile(token_list, albums, is_in_album, is_in_track, current_album, current_track, current_character);
		

		/*for (int i = 0; i < token_list.size(); i++) {
			System.out.println(token_list.get(i));
		}*/
		
		for(int i = 0; i < albums.size(); i++){
			System.out.println(albums.get(i));
		}	

	}
}
