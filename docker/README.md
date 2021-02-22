
# Dockerize the ARGP Shiny app 

Build the docker image of ARGP shiny framework 

```
docker build -t app .
```

Run the image using e.g. 

```
docker run -p 3839:3838 -it app 
```

and accessed the app in a browser at http://127.0.0.1:3839