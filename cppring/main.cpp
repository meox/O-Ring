#include <chrono>
#include <cstdlib>
#include <future>
#include <iostream>
#include <memory>
#include <vector>

#include "boost/fiber/future.hpp"
#include "boost/fiber/unbuffered_channel.hpp"


 int main(int argc, char* argv[]) {
    using Message = uint64_t;
    using Channel = boost::fibers::unbuffered_channel<Message>;
    using namespace std::chrono;

    if (argc != 3) {
        std::cerr << "Invalid args: ./ring NumberOfNodes NumberOfTrips, both args must be > 0" << std::endl;
        return 1;
    }

    const unsigned n = std::atoi(argv[1]);
    const unsigned m = std::atoi(argv[2]);

    if (n < 1 || m < 1) {
        std::cerr << "Invalid args: ./ring NumberOfNodes NumberOfTrips, both args must be > 0" << std::endl;
        return 1;
    }

    // Initialization
    const auto initStart = high_resolution_clock::now();

    auto node = [] (Channel& src, Channel& dst) {
        Message x;
        while (src.pop(x) == boost::fibers::channel_op_status::success) {
            dst.push(x + 1);
        }
    };

    Channel startChannel;
    std::vector<Channel> v{n};
    std::vector<boost::fibers::future<void>> futures;
    futures.reserve(n);

    auto src = &startChannel;
    for (size_t i = 0; i < v.size(); ++i) {
        auto dst = &v[i];
        futures.emplace_back(boost::fibers::async(
                                 boost::fibers::launch::post,
                                 std::allocator_arg_t{},
                                 boost::fibers::fixedsize_stack{10 * 1024},
                                 [src, dst, &node] { node(*src, *dst); }
        ));
        src = dst;
    }

    // Messaging
    const auto msgStart = high_resolution_clock::now();

    Message total{};
    for (size_t i = 0; i < m; ++i) {
        startChannel.push(0);
        Message e;
        if (v.back().pop(e) == boost::fibers::channel_op_status::success)
            total += e;
    }


    const auto msgDur = high_resolution_clock::now() - msgStart;

    // Output
    const auto initDur = msgStart - initStart;
    if (total != n * m) {
        std::cout << "Wrong out" << std::endl;
        return 1;
    }

    std::cout << duration_cast<milliseconds>(initDur).count() << " " <<
                 duration_cast<milliseconds>(msgDur).count() << " " <<
                 n << " " << m << std::endl;

    startChannel.close();
    for (auto& c : v)
        c.close();

    for (auto& f : futures)
        f.wait();
}
