use async_std::sync::{channel, Receiver, Sender};
use async_std::task;
use std::env;
use std::time::Instant;
fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 3 {
        panic!("./ring N:<number of process> M:<trips>");
    }

    let n: u64 = args[1].parse().expect("Cannot parse number of process");
    let m: u64 = args[2].parse().expect("Cannot parse number of trips");

    let start = Instant::now();
    let (tx, rx) = create_ring(n);
    let creation_time = start.elapsed();

    let (elapsed_time, total) = task::block_on(async {
        let start = Instant::now();
        let mut total = 0;
        for _ in 0..m {
            tx.send(0).await;
            let received = rx.recv().await.expect("It should receive a value");
            total += received;
        }
        (start.elapsed(), total)
    });

    if total != n * m {
        panic!("Ring failed")
    }

    println!(
        "{:?} {:?} {} {}",
        creation_time.as_millis(),
        elapsed_time.as_millis(),
        n,
        m
    );
}

fn create_ring(n: u64) -> (Sender<u64>, Receiver<u64>) {
    let (tx, mut rx) = channel(1);

    for _ in 0..n {
        let (t, r): (Sender<u64>, Receiver<u64>) = channel(1);
        let inner_r = rx.clone();
        task::spawn(async move {
            loop {
                match inner_r.recv().await {
                    Some(m) => {
                        t.send(m + 1).await;
                    }
                    None => {
                        break;
                    }
                }
            }
        });
        rx = r;
    }
    return (tx, rx);
}
