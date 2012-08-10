Delphi REST Client API
======================

A Delphi REST client API to consume REST services written in any programming language.

The API was tested in Delphi 7, XE e XE2, and need [Indy 10](http://www.indyproject.org/index.en.aspx) to work fine.

Serialization / deserialization is transparent. The objects are transported as JSON format, and the i am using [SuperObject](http://code.google.com/p/superobject/) as Json framework.

For serialization to work well, the object must be declared as follows, with public fields:
     
    TPerson = class(TObject)
    public 
      (* Reflect the java object field names, case-sensitive *)
      id: Integer;
      name: String;
      email: String;

      (* utility function *)
      class function NewFrom(Id: Integer; Name, EMail: String): TPerson;
    end;

Samples
----------------

 - **GET**

		vLista := RestClient.Resource('http://localhost:8080/java-rest-server/rest/persons')
                        .Accept(RestUtils.MediaType_Json)
		                    .GetAsList<TPerson>(TPerson);

 - **GET ONE**

	    vPerson := RestClient.Resource('http://localhost:8080/java-rest-server/rest/person/1')
		                     .Accept(RestUtils.MediaType_Json)
		                     .Get<TPerson>(TPerson);

 - **POST**

          vPerson := TPerson.NewFrom(123, 'Fabricio', 'fabricio.colombo.mva@gmail.com');
          RestClient.Resource('http://localhost:8080/java-rest-server/rest/person')
                    .Accept(RestUtils.MediaType_Json)
                    .ContentType(RestUtils.MediaType_Json)
                    .Post<TPerson>(vPerson);
		
 - **PUT**

          vPerson := //Load person
          vPerson.Email := 'new@email.com';
          RestClient.Resource('http://localhost:8080/java-rest-server/rest/person')
                    .Accept(RestUtils.MediaType_Json)
                    .ContentType(RestUtils.MediaType_Json)
                    .Put<TPerson>(vPerson);
									 
Java Rest Server
----------------
	
The java project is only for test purpose and has built using [Maven](http://maven.apache.org) and [Jersey](http://jersey.java.net), so it's needed have installed the JRE 6+ (Java Runtime Environment) and Maven 2 to build and run the application. The Maven bin directory must be included in Windows Path environment variable.

After install Java and Maven just run 'start-java-server.bat' to start the application and 'stop-java-server.bat' to shut down them.
	
When 'start-java-server.bat' is first run maven dependencies will be downloaded, and it may take a while.




Using this tool
---------------

This page lets you create HTML by entering text in a simple format that's easy to read and write.

  - Type Markdown text in the left window
  - See the HTML in the right

Markdown is a lightweight markup language based on the formatting conventions that people naturally use in email.  As [John Gruber] writes on the [Markdown site] [1]:

> The overriding design goal for Markdown's
> formatting syntax is to make it as readable 
> as possible. The idea is that a
> Markdown-formatted document should be
> publishable as-is, as plain text, without
> looking like it's been marked up with tags
> or formatting instructions.

This document is written in Markdown; you can see the plain-text version on the left.  To get a feel for Markdown's syntax, type some text into the left window and watch the results in the right.  You can see a Markdown syntax guide by switching the right-hand window from *Preview* to *Syntax Guide*.

Showdown is a Javascript port of Markdown.  You can get the full [source code] by clicking on the version number at the bottom of the page.

**Start with a [blank page] or edit this document in the left window.**

  [john gruber]: http://daringfireball.net/
  [1]: http://daringfireball.net/projects/markdown/
  [source code]: http://www.attacklab.net/showdown-v0.9.zip
  [blank page]: ?blank=1 "Clear all text"