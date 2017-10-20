(in-package :conf)
(defun pom (group-id artifact-id) 
  (node '|project| 
        :|xmlns| "http://maven.apache.org/POM/4.0.0" :|xmlns:xsi| "http://www.w3.org/2001/XMLSchema-instance" 
        :|xsi:schemaLocation| "http://maven.apache.org/POM/4.0.0 http://maven.apache.org/maven-v4_0_0.xsd"
        (node '|modelVersion| (simple "4.0.0")) 
        (node '|groupId| (simple (apply #'mkstr (interleave group-id ".")))) 
        (node '|artifactId| (simple (mkstr artifact-id)))
        (node '|packaging| (simple "war")) 
        (node '|version| (simple "0.0.1-SNAPSHOT")) 
        (node '|name| (simple (mkstr artifact-id))) 
        (node '|url| (simple "http://maven.apache.org")) 
        (node '|dependencies|
              (node '|dependency|
                    (node '|groupId| (simple "junit")) 
                    (node '|artifactId| (simple "junit")) 
                    (node '|version| (simple "3.8.1")) 
                    (node '|scope| (simple "test")))
              (node '|dependency|
                    (node '|groupId| (simple "javax.servlet")) 
                    (node '|artifactId| (simple "javax.servlet-api")) 
                    (node '|version| (simple "3.1.0")) 
                    (node '|scope| (simple "provided")))
              (node '|dependency|
                    (node '|groupId| (simple "javax")) 
                    (node '|artifactId| (simple "javaee-api")) 
                    (node '|version| (simple "7.0"))))
        (node '|build|
              (node '|finalName| (simple (mkstr artifact-id))) 
                      
              (node '|plugins|
                    (node '|plugin|
                          (node '|artifactId| (simple "maven-compiler-plugin")) 
                          (node '|version| (simple "3.0")) 
                          (node '|configuration|
                                (node '|source| (simple "1.8")) 
                                (node '|target| (simple "1.8"))))))))

(defun web (group-id artifact-id services)
  (let ((beans (apply #'mkstr (interleave (mapcar (lambda (service) (mkstr "java:global/" artifact-id "/" (upper-camel (synth :name service)) "!" (apply #'mkstr (interleave group-id ".")) "." artifact-id ".service." (upper-camel (synth :name service))))
                                                  services) ",")))) 
    (node '|web-app| 
          :|xmlns:xsi| "http://www.w3.org/2001/XMLSchema-instance"
          :|xmlns| "http://java.sun.com/xml/ns/javaee"
          :|xsi:schemaLocation| "http://java.sun.com/xml/ns/javaee http://java.sun.com/xml/ns/javaee/web-app_3_0.xsd"
          :|version| "3.0"
          (node '|servlet| 
                (node '|servlet-name| (simple "javax.ws.rs.core.Application"))
                (node '|load-on-startup| (simple "1")))
          (node '|servlet-mapping| 
                (node '|servlet-name| (simple "javax.ws.rs.core.Application"))
                (node '|url-pattern| (simple "/services/*")))
          (node '|context-param| 
                (node '|param-name| (simple "resteasy.jndi.resources"))
                (node '|param-value| (simple beans)))
          (node '|context-param| 
                (node '|param-name| (simple "resteasy.scan"))
                (node '|param-value| (simple "false")))
          (node '|context-param| 
                (node '|param-name| (simple "resteasy.scan.providers"))
                (node '|param-value| (simple "false")))
          (node '|context-param| 
                (node '|param-name| (simple "resteasy.scan.resources"))
                (node '|param-value| (simple "false"))))))
(defun beans () 
  (node '|beans| 
        :|xmlns| "http://xmlns.jcp.org/xml/ns/javaee"
        :|xmlns:xsi| "http://www.w3.org/2001/XMLSchema-instance"
        :|xsi:schemaLocation| "http://xmlns.jcp.org/xml/ns/javaee http://xmlns.jcp.org/xml/ns/javaee/beans_1_1.xsd"
        :|version| "1.1"
        :|bean-discovery-mode| "all"))

(defun persistence (entities)
  (node '|persistence| :|version| "2.1"
        :|xmlns| "http://xmlns.jcp.org/xml/ns/persistence" 
        :|xmlns:xsi| "http://www.w3.org/2001/XMLSchema-instance"
        :|xsi:schemaLocation| "http://xmlns.jcp.org/xml/ns/persistence http://xmlns.jcp.org/xml/ns/persistence/persistence_2_1.xsd" 
	(node '|persistence-unit| :|name| "h2"
              (node '|provider| (simple "org.hibernate.ejb.HibernatePersistence"))
              (node '|jta-data-source| (simple "java:jboss/datasources/ExampleDS"))
              (node '|properties| 
                    (node '|property| :|name| "javax.persistence.jdbc.url"
                          :|value| "jdbc:h2:mem:test;INIT=RUNSCRIPT FROM 'classpath:create.sql'\;RUNSCRIPT FROM 'classpath:data.sql'")
                    (node '|property| :|name| "javax.persistence.jdbc.driver"
                          :|value| "org.h2.Driver")
                    (node '|property| :|name| "hibernate.dialect"
                          :|value| "org.hibernate.dialect.H2Dialect")
                    (node '|property| :|name| "hibernate.max_fetch_depth"
                          :|value| "3")
                    (node '|property| :|name| "hibernate.show_sql"
                          :|value| "true")))))
