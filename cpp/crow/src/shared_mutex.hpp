/**
 * Shared Mutex from:
 * http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2007/n2406.html#shared_mutex
 */

using namespace std;

class shared_mutex
{
    mutex    mut_;
    condition_variable gate1_;
    condition_variable gate2_;
    unsigned state_;

    static const unsigned write_entered_ = 1U << (sizeof(unsigned)*CHAR_BIT - 1);
    static const unsigned n_readers_ = ~write_entered_;

public:

    shared_mutex() : state_(0) {}

// Exclusive ownership

    void lock();
    bool try_lock();
    void unlock();

// Shared ownership

    void lock_shared();
    bool try_lock_shared();
    void unlock_shared();
};

// Exclusive ownership

void
shared_mutex::lock()
{
    unique_lock<mutex> lk(mut_);
    while (state_ & write_entered_)
        gate1_.wait(lk);
    state_ |= write_entered_;
    while (state_ & n_readers_)
        gate2_.wait(lk);
}

bool
shared_mutex::try_lock()
{
    unique_lock<mutex> lk(mut_, try_to_lock);
    if (lk.owns_lock() && state_ == 0)
    {
        state_ = write_entered_;
        return true;
    }
    return false;
}

void
shared_mutex::unlock()
{
    {
    lock_guard<mutex> _(mut_);
    state_ = 0;
    }
    gate1_.notify_all();
}

// Shared ownership

void
shared_mutex::lock_shared()
{
    unique_lock<mutex> lk(mut_);
    while ((state_ & write_entered_) || (state_ & n_readers_) == n_readers_)
        gate1_.wait(lk);
    unsigned num_readers = (state_ & n_readers_) + 1;
    state_ &= ~n_readers_;
    state_ |= num_readers;
}

bool
shared_mutex::try_lock_shared()
{
    unique_lock<mutex> lk(mut_, try_to_lock);
    unsigned num_readers = state_ & n_readers_;
    if (lk.owns_lock() && !(state_ & write_entered_) && num_readers != n_readers_)
    {
        ++num_readers;
        state_ &= ~n_readers_;
        state_ |= num_readers;
        return true;
    }
    return false;
}

void
shared_mutex::unlock_shared()
{
    lock_guard<mutex> _(mut_);
    unsigned num_readers = (state_ & n_readers_) - 1;
    state_ &= ~n_readers_;
    state_ |= num_readers;
    if (state_ & write_entered_)
    {
        if (num_readers == 0)
            gate2_.notify_one();
    }
    else
    {
        if (num_readers == n_readers_ - 1)
            gate1_.notify_one();
    }
}
