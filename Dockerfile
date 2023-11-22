FROM openjdk:11-jdk-slim as builder
ENV DEBIAN_FRONTEND noninteractive
RUN apt-get update -y && apt-get install -y curl
COPY ./target/scala-2.12/auction-coin-1.1.jar /auction-coin.jar
WORKDIR /
CMD ["java", "-jar", "/ergo.jar"]
FROM openjdk:11-jre-slim
RUN adduser --disabled-password --home /home/ergo --uid 9052 --gecos "ErgoPlatform" ergo && \
    install -m 0750 -o ergo -g ergo -d /home/ergo/.ergo
COPY --from=builder /auction-coin.jar /home/ergo/auction-coin.jar
USER ergo
EXPOSE 8080
WORKDIR /home/ergo
VOLUME ["/home/ergo/.ergo"]
ENTRYPOINT java -jar -Dconfig.file=application.conf /home/ergo/auction-coin.jar
CMD []
