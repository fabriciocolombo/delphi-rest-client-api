Delphi REST Client API
======================

A Delphi REST client API to consume REST services written in any programming language.

The API was tested in Delphi 7, XE, XE2, XE3 and XE4. It is also compatible with Mac OSX and iOS.

Connection Layer
---
There are a IHttpConnection interface to abstract the real Http conection. This interface currently have two implementations, using  [Indy 10](http://www.indyproject.org/index.en.aspx), [WinHTTP](http://msdn.microsoft.com/en-us/library/windows/desktop/aa382925.aspx) and [WinInet](http://msdn.microsoft.com/en-us/library/windows/desktop/aa383630.aspx).

Indy 9 does not handles HTTP response codes correctly, then if you are using Delphi 7, you must update your indy library to version 10 or use WinHttp (recommended). To disable indy support comment the compiler directive ``{.$DEFINE USE_INDY}`` in ``DelphiRest.inc`` file.

Serialization/Desserialization
--	
The objects are streamed as JSON format, and SuperObject was chosen for deal with json transformation. SuperObject it's lighter  and less intrusive than DBX framework. He too have a good support to serialization through TSuperObjectHelper class helper.

The framework SuperObject is not compatible with OSX and iOS, for such is used DBXJson framework, which has no compatibility issues. By default SuperObject still used, enabling DBXJson only on MacOS environment.
You can configure DBXJson  as default framework disabling compiler directive ``USE_SUPER_OBJECT`` in the file ``DelphiRest.inc``.

**Serialization is not available (yet) for Delphi 7.**


For serialization work fine, the object must be declared as follows, with public fields. How public fields are a bad smell(IMHO), the serialization process will be improved to read `properties` too, and give some configuration options.
     
    TPerson = class(TObject)
    public 
      (* Reflect the server side object field names, for Java must be case-sensitive *)
      id: Integer;
      name: String;
      email: String;

      (* Static constructor *)
      class function NewFrom(Id: Integer; Name, EMail: String): TPerson;
    end;

Samples
----------------

 - **GET**
                
        var
          vList : TList<TPerson>;
        begin
          vList := RestClient.Resource('http://localhost:8080/java-rest-server/rest/persons')
                             .Accept(RestUtils.MediaType_Json)
                             .Get<TList<TPerson>>();

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