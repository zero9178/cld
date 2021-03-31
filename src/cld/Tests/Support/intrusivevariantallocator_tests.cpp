#include <catch.hpp>

#include <cld/Support/AbstractIntrusiveVariant.hpp>
#include <cld/Support/IntrVarAllocator.hpp>

#include <queue>

namespace
{
class Base : public cld::AbstractIntrusiveVariant<class Derived1, class DestrSideEffect, class Tree>
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

class Tree : public Base
{
public:
    cld::IVAUniquePtr<Base> sub;

    Tree() : Base(std::in_place_type<Tree>) {}
};

} // namespace

TEST_CASE("IntrusiveVariantAllocator", "[IVA]")
{
    SECTION("Simple allocation")
    {
        cld::IntrVarAllocator<Base> allocator;
        auto derived1 = allocator.alloc<Derived1>(5, 3.0);
        CHECK(derived1->is<Derived1>());
        CHECK(derived1->size == 5);
        CHECK(derived1->thing == 3.0);
    }
    SECTION("Does deallocate all on destruction")
    {
        DestrSideEffect::i = 0;
        {
            cld::IntrVarAllocator<Base> allocator;
            CHECK(DestrSideEffect::i == 0);
            auto derived1 = allocator.alloc<DestrSideEffect>();
            CHECK(derived1->is<DestrSideEffect>());
            CHECK(DestrSideEffect::i == 1);
        }
        CHECK(DestrSideEffect::i == -1);
    }
    SECTION("Can manually deallocate")
    {
        DestrSideEffect::i = 0;
        cld::IntrVarAllocator<Base> allocator;
        CHECK(DestrSideEffect::i == 0);
        auto derived1 = allocator.alloc<DestrSideEffect>();
        CHECK(derived1->is<DestrSideEffect>());
        CHECK(DestrSideEffect::i == 1);
        allocator.destroy(derived1);
        CHECK(DestrSideEffect::i == -1);
    }
    SECTION("Random alloc and dealloc")
    {
        std::queue<Base*> bases;
        struct Operation
        {
            bool alloc;
            std::size_t size;
            float thing;
        };
        std::vector<Operation> operations = {
            {false, 0, 0.0f},
            {true, 456663322, 79.08536534226378},
            {false, 0, 0.0f},
            {true, 874589565, 38.29703042625565},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {true, 115330298, 94.71585671130644},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {true, 514088382, 43.653231128586995},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {true, 475517242, 71.93646804971986},
            {false, 0, 0.0f},
            {true, 291212866, 72.00777883247625},
            {true, 87540991, 90.98056825852453},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {true, 346223279, 56.723576513920264},
            {true, 649154546, 24.56458897035569},
            {true, 750096269, 52.716998092806634},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {true, 20218084, 34.9683132077394},
            {true, 308160019, 80.45637572770787},
            {true, 87519731, 66.62229083127346},
            {true, 546543516, 59.57728517469431},
            {true, 720604832, 90.69178291804826},
            {false, 0, 0.0f},
            {true, 124383238, 10.557105478066456},
            {true, 112742409, 93.15979828162264},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {true, 513950300, 6.064549142096096},
            {false, 0, 0.0f},
            {true, 6875917, 69.81582332688575},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {true, 262117613, 91.39707893210361},
            {true, 942587941, 23.819841757387337},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {true, 590832518, 33.02407641667122},
            {true, 893007975, 82.7363811157001},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {true, 19786800, 41.539253251032775},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {true, 137767822, 50.899685485667334},
            {false, 0, 0.0f},
            {true, 344726182, 57.0083949013544},
            {true, 136217123, 58.89462671205446},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {true, 13310977, 22.36627590015554},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {true, 984254327, 55.33173408272621},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {true, 302362803, 40.260621642198615},
            {false, 0, 0.0f},
            {true, 790180710, 1.0415301318483596},
            {true, 421341058, 50.859671500637106},
            {true, 549064969, 15.138555869447273},
            {true, 346898092, 43.46124890869215},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {true, 237359187, 8.25809376070654},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {true, 807252757, 58.991708194732865},
            {true, 790672629, 25.20025879630114},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {true, 581224451, 69.2237452015172},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {true, 699406750, 59.9257331396659},
            {false, 0, 0.0f},
            {true, 940773626, 17.35183658719023},
            {true, 643913496, 79.10649215418256},
            {true, 254817167, 71.27347193377952},
            {false, 0, 0.0f},
            {true, 241213826, 77.32176694803809},
            {true, 523623099, 2.0675897442048394},
            {true, 236681184, 77.93544653414646},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {true, 992146488, 23.593244227299678},
            {true, 877109889, 69.68861353441984},
            {true, 528728059, 60.40181550768939},
            {false, 0, 0.0f},
            {true, 847065062, 10.581489413524908},
            {false, 0, 0.0f},
            {true, 525574587, 9.303111329045654},
            {false, 0, 0.0f},
            {true, 467657033, 7.08037272625662},
            {true, 29000475, 5.144519216763759},
            {false, 0, 0.0f},
            {true, 872294711, 12.906341413891095},
            {true, 961737995, 1.881285611415906},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {true, 340169290, 85.07337272363705},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {true, 655224904, 41.161444547953806},
            {true, 776953190, 46.19833883851668},
            {false, 0, 0.0f},
            {true, 165657222, 40.73984508157131},
            {true, 508936328, 82.65643738821097},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {true, 382095879, 79.59917985811528},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {true, 295304137, 88.7056461654852},
            {true, 723033788, 54.86509776249257},
            {true, 851788046, 13.427332385842051},
            {false, 0, 0.0f},
            {true, 503388495, 17.84000759256017},
            {true, 142479698, 65.71854570780566},
            {true, 365898782, 72.1657577783794},
            {true, 822471502, 22.602441537219804},
            {true, 140155647, 54.41985346818574},
            {true, 451454515, 90.59274534141888},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {true, 56681782, 17.22303976386241},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {true, 732468054, 86.04407981642757},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {true, 226817124, 15.979579879456141},
            {true, 347082825, 89.72252480116298},
            {true, 457035826, 16.025775188050787},
            {false, 0, 0.0f},
            {true, 479635661, 89.25977106378191},
            {false, 0, 0.0f},
            {true, 789738559, 79.97627060587347},
            {true, 691395301, 77.56735265201296},
            {false, 0, 0.0f},
            {true, 656723180, 47.912475598596195},
            {true, 157922872, 74.3362868679244},
            {false, 0, 0.0f},
            {true, 556490704, 24.080354637931173},
            {false, 0, 0.0f},
            {true, 924109100, 17.373829763899252},
            {true, 921050986, 21.44935126653712},
            {true, 319436713, 66.26854120933106},
            {true, 89127930, 7.390169466811568},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {true, 543095222, 47.25418943350688},
            {true, 738933308, 21.020372208384074},
            {false, 0, 0.0f},
            {true, 94090333, 50.256579089592755},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {true, 745028281, 9.61996505027296},
            {true, 183678614, 47.4808263406969},
            {false, 0, 0.0f},
            {true, 456434226, 70.5755671707603},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
        };

        cld::IntrVarAllocator<Base> allocator;
        for (auto& iter : operations)
        {
            if (!iter.alloc)
            {
                if (!bases.empty())
                {
                    allocator.destroy(bases.front());
                    bases.pop();
                }
                continue;
            }
            bases.push(allocator.alloc<Derived1>(iter.size, iter.thing));
            CHECK(bases.back()->is<Derived1>());
            CHECK(bases.back()->as<Derived1>().size == iter.size);
            CHECK(bases.back()->as<Derived1>().thing == iter.thing);
        }
    }
    SECTION("Large amount")
    {
        cld::IntrVarAllocator<Base> allocator;
        for (std::size_t i = 0; i < 1000; i++)
        {
            auto derived1 = allocator.alloc<Derived1>(5, 3.0);
            CHECK(derived1->is<Derived1>());
            CHECK(derived1->size == 5);
            CHECK(derived1->thing == 3.0);
        }
    }
    SECTION("Unique ptr")
    {
        DestrSideEffect::i = 0;
        cld::IVAUniquePtr<Base, DestrSideEffect> ptr;
        CHECK(DestrSideEffect::i == 0);
        cld::IntrVarAllocator<Base> allocator;
        ptr = allocator.allocUnique<DestrSideEffect>();
        CHECK(ptr->is<DestrSideEffect>());
        CHECK(DestrSideEffect::i == 1);
        ptr.reset();
        CHECK(DestrSideEffect::i == -1);
    }
    SECTION("No double destruction")
    {
        cld::IntrVarAllocator<Base> allocator;
        auto firstTree = allocator.allocUnique<Tree>();
        auto secondTree = allocator.allocUnique<Tree>();
        secondTree->sub = std::move(firstTree);
        secondTree.release(); // LEAK so to say, but allocator will clean it up
    }
}
