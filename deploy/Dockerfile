FROM fpco/stack-build:lts-10.4

COPY planogramming-server /
COPY planogramming-hs-client /
COPY planogram-db /planogram-db

ENTRYPOINT ./planogramming-server 3050
EXPOSE 3050
