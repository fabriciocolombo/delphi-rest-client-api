Delphi REST Client API
======================

A Delphi REST client API to consume REST services written in any programming language.

The API was tested in Delphi 7, XE e XE2, and need [Indy 10](http://www.indyproject.org/index.en.aspx) to work fine.

Samples
----------------


 RestClient.Resource('http://localhost:8080/java-rest-server/rest/persons')
           .Accept(RestUtils.MediaType_Json)
           .Get();
											 
Java Rest Server
----------------
	
The java project is only for test purpose and has built using [Maven](http://maven.apache.org) and [Jersey](http://jersey.java.net), so it's needed have installed the JRE 6+ (Java Runtime Environment) and Maven 2 to build and run the application. The Maven bin directory must be included in Windows Path environment variable.

After install Java and Maven just run 'start-java-server.bat' to start the application and 'stop-java-server.bat' to shut down them.
	
When 'start-java-server.bat' is first run maven dependencies will be downloaded, and it may take a while.

