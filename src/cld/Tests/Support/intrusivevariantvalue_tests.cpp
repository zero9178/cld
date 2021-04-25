#include <catch.hpp>

#include <cld/Support/AbstractIntrusiveVariant.hpp>
#include <cld/Support/IntrVarValue.hpp>

namespace
{
class TrivBase : public cld::AbstractIntrusiveVariant<TrivBase, class TrivDerived1, class TrivDerived2>
{
protected:
    template <class T>
    TrivBase(std::in_place_type_t<T>) : AbstractIntrusiveVariant(std::in_place_type<T>)
    {
    }
};

class TrivDerived1 : public TrivBase
{
public:
    std::size_t size;
    float thing;

    TrivDerived1(std::size_t size, float thing) : TrivBase(std::in_place_type<TrivDerived1>), size(size), thing(thing)
    {
    }
};

class TrivDerived2 : public TrivBase
{
public:
    TrivDerived2() : TrivBase(std::in_place_type<TrivDerived2>) {}
};

} // namespace

static_assert(std::is_trivially_copyable_v<cld::IntrVarValue<TrivBase>>);
static_assert(std::is_nothrow_copy_constructible_v<cld::IntrVarValue<TrivBase>>);
static_assert(std::is_trivially_copy_assignable_v<cld::IntrVarValue<TrivBase>>);
static_assert(std::is_nothrow_copy_assignable_v<cld::IntrVarValue<TrivBase>>);
static_assert(std::is_trivially_move_constructible_v<cld::IntrVarValue<TrivBase>>);
static_assert(std::is_nothrow_move_constructible_v<cld::IntrVarValue<TrivBase>>);
static_assert(std::is_trivially_move_assignable_v<cld::IntrVarValue<TrivBase>>);
static_assert(std::is_nothrow_move_assignable_v<cld::IntrVarValue<TrivBase>>);
static_assert(std::is_trivially_destructible_v<cld::IntrVarValue<TrivBase>>);

namespace
{
class Base
    : public cld::AbstractIntrusiveVariant<Base, class Derived1, class DestrSideEffect, class MoveConstrSideEffect>
{
protected:
    template <class T>
    Base(std::in_place_type_t<T>) : AbstractIntrusiveVariant(std::in_place_type<T>)
    {
    }
};

class Derived1 : public Base
{
public:
    std::size_t size;
    float thing;

    Derived1(std::size_t size, float thing) : Base(std::in_place_type<Derived1>), size(size), thing(thing) {}
};

class DestrSideEffect : public Base
{
public:
    inline thread_local static int i = 0;

    DestrSideEffect() : Base(std::in_place_type<DestrSideEffect>)
    {
        i = 1;
    }

    ~DestrSideEffect()
    {
        i = -1;
    }
};

class MoveConstrSideEffect : public Base
{
public:
    inline thread_local static int i = 0;

    MoveConstrSideEffect() : Base(std::in_place_type<MoveConstrSideEffect>) {}

    MoveConstrSideEffect(MoveConstrSideEffect&& rhs) noexcept : Base(std::move(rhs))
    {
        i++;
    }

    MoveConstrSideEffect& operator=(MoveConstrSideEffect&&) noexcept = default;

    MoveConstrSideEffect(const MoveConstrSideEffect&) = delete;
    MoveConstrSideEffect& operator=(const MoveConstrSideEffect&) = delete;
};

} // namespace

TEST_CASE("IntrVarValue non trivial", "[IVV]")
{
    SECTION("Simple usage")
    {
        cld::IntrVarValue value = Derived1(5, 3.0);
        REQUIRE(value->is<Derived1>());
        CHECK(value->as<Derived1>().size == 5);
        CHECK(value->as<Derived1>().thing == 3.0);
    }
    SECTION("In place construction")
    {
        cld::IntrVarValue value(std::in_place_type<Derived1>, 5, 3.0);
        REQUIRE(value->is<Derived1>());
        CHECK(value->as<Derived1>().size == 5);
        CHECK(value->as<Derived1>().thing == 3.0);
    }
    SECTION("Emplace construction")
    {
        cld::IntrVarValue value(std::in_place_type<Derived1>, 5, 3.0);
        REQUIRE(value->is<Derived1>());
        CHECK(value->as<Derived1>().size == 5);
        CHECK(value->as<Derived1>().thing == 3.0);
        auto& derived = value.emplace<Derived1>(8, 5.0);
        CHECK(derived.size == 8);
        CHECK(derived.thing == 5.0);
    }
    SECTION("Move assignable once")
    {
        cld::IntrVarValue value = Derived1(5, 3.0);
        MoveConstrSideEffect::i = 0;
        value = MoveConstrSideEffect();
        CHECK(MoveConstrSideEffect::i == 1);
    }
    SECTION("Copyable from T")
    {
        auto d = Derived1(5, 3.0);
        cld::IntrVarValue value = d;
        REQUIRE(value->is<Derived1>());
        value->as<Derived1>().size = 3;
        CHECK(value->as<Derived1>().size == 3);
        value->as<Derived1>().thing = 2;
        CHECK(value->as<Derived1>().thing == 2);
        value = d;
        CHECK(value->as<Derived1>().size == 5);
        CHECK(value->as<Derived1>().thing == 3.0);
    }
}
