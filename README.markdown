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
                
        var
          vLista : TList<TPerson>;
        begin
          vLista := RestClient.Resource('http://localhost:8080/java-rest-server/rest/persons')
                            .Accept(RestUtils.MediaType_Json)
                            .GetAsList<TPerson>();

 - **GET ONE**

        var
          vPerson : TPerson;
        begin
          vPerson := RestClient.Resource('http://localhost:8080/java-rest-server/rest/person/1')
		                     .Accept(RestUtils.MediaType_Json)
		                     .Get<TPerson>();

 - **POST**

        var
          vPerson : TPerson;
        begin
          vPerson := TPerson.NewFrom(123, 'Fabricio', 'fabricio.colombo.mva@gmail.com');          
          RestClient.Resource('http://localhost:8080/java-rest-server/rest/person')
                    .Accept(RestUtils.MediaType_Json)
                    .ContentType(RestUtils.MediaType_Json)
                    .Post<TPerson>(vPerson);
		
 - **PUT**

        var
          vPerson : TPerson;
        begin
          vPerson := //Load person
          vPerson.Email := 'new@email.com';
          RestClient.Resource('http://localhost:8080/java-rest-server/rest/person')
                    .Accept(RestUtils.MediaType_Json)
                    .ContentType(RestUtils.MediaType_Json)
                    .Put<TPerson>(vPerson);

 - **DELETE**

        var
          vPerson : TPerson;
        begin
          vPerson := //Load person
          RestClient.Resource('http://localhost:8080/java-rest-server/rest/person')
                    .Accept(RestUtils.MediaType_Json)
                    .ContentType(RestUtils.MediaType_Json)
                    .Delete(vPerson);
			
 - **GET AS DATASET**

  The fields need be predefined.

        var
          vDataSet: TClientDataSet;
        begin
          vDataSet := TClientDataSet.Create(nil);
          try
            TDataSetUtils.CreateField(vDataSet, ftInteger, 'id');
            TDataSetUtils.CreateField(vDataSet, ftString, 'name', 100);
            TDataSetUtils.CreateField(vDataSet, ftString, 'email', 100);
            vDataSet.CreateDataSet;

           RestClient.Resource(CONTEXT_PATH + 'persons')
                      .Accept(RestUtils.MediaType_Json)
                      .GetAsDataSet(vDataSet);
          finally
            vDataSet.Free;
          end;
		
 - **GET AS DYNAMIC DATASET**

  The fields are created dynamically according to the returned content.

        var
          vDataSet: TDataSet;
        begin
          vDataSet := RestClient.Resource(CONTEXT_PATH + 'persons')
                                .Accept(RestUtils.MediaType_Json)
                                .GetAsDataSet();        
          try
            //Do something
          finally
            vDataSet.Free;
          end;
				 
Java Rest Server
----------------
	
The java project is only for test purpose and has built using [Maven](http://maven.apache.org) and [Jersey](http://jersey.java.net), so it's needed have installed the JRE 6+ (Java Runtime Environment) and Maven 2 to build and run the application. The Maven bin directory must be included in Windows Path environment variable.

After install Java and Maven just run 'start-java-server.bat' to start the application and 'stop-java-server.bat' to shut down them.
	
When 'start-java-server.bat' is first run maven dependencies will be downloaded, and it may take a while.