package com.alexeyshmalko;

import scala.concurrent.duration.Duration;
import java.util.concurrent.TimeUnit;

import akka.actor.UntypedActor;
import akka.event.Logging;
import akka.event.LoggingAdapter;

import akka.actor.ActorSystem;
import akka.actor.ActorRef;
import akka.actor.Props;
import akka.actor.Cancellable;

public class App {
    public static void main(String[] args) {
        final ActorSystem system = ActorSystem.create("kplab5");
        final ActorRef producer = system.actorOf(Props.create(ProducerActor.class));
    }
}

class ConsumerActor extends UntypedActor {
    public void onReceive(Object message) throws Exception {
        if (message instanceof Long) {
            System.out.println("Receive: " + message);
        } else {
            unhandled(message);
        }
    }
}

class ProducerActor extends UntypedActor {
    private final ActorRef consumer = getContext().actorOf(Props.create(ConsumerActor.class));

    private final Cancellable tick = getContext().system().scheduler().schedule(
        Duration.create(1, TimeUnit.SECONDS),
        Duration.create(1, TimeUnit.SECONDS),
        getSelf(), "tick", getContext().dispatcher(), null);

    private int counter = 0;

    @Override
    public void postStop() {
        tick.cancel();
    }

    @Override
    public void onReceive(Object message) throws Exception {
        if (message.equals("tick")) {
            System.out.println("Send:    " + counter);
            consumer.tell(new Long(counter++), getSelf());
        } else {
            unhandled(message);
        }
    }
}
