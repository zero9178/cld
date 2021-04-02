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
            {true, 116629187, 73.9440806051959},
            {true, 277612820, 99.09547311576395},
            {true, 759545022, 3.309238435607314},
            {true, 252926135, 73.61982694682474},
            {true, 733364883, 93.6647266088162},
            {true, 760034269, 16.896271695412075},
            {true, 502155261, 83.7306164048671},
            {true, 678015504, 41.96613689884083},
            {true, 964011958, 14.894994448687825},
            {true, 229024193, 84.743393408499},
            {true, 690609869, 31.64428631906867},
            {true, 663170267, 62.80406548803369},
            {true, 809683543, 50.334987664403016},
            {true, 85605782, 69.4344477991786},
            {true, 792806898, 77.95372929739754},
            {true, 930751462, 73.00433677071945},
            {true, 160541283, 3.4544853784154297},
            {true, 20488933, 24.00270666817804},
            {true, 83645775, 63.112514531807385},
            {true, 720681039, 20.588942704253505},
            {true, 937887264, 6.370476666393468},
            {true, 283774499, 69.17255496327057},
            {true, 904687810, 91.00356977485835},
            {false, 0, 0.0f},
            {true, 266666161, 91.72537688339602},
            {true, 715190164, 51.191015939760874},
            {true, 701922832, 50.63408667901075},
            {true, 793357734, 87.26828544085252},
            {true, 963284151, 34.179174079132835},
            {false, 0, 0.0f},
            {true, 932508283, 86.58920572637314},
            {false, 0, 0.0f},
            {true, 73738180, 11.758922565192327},
            {true, 520193096, 99.13015702468378},
            {true, 335694452, 70.1172733959761},
            {true, 6520254, 35.16183877952068},
            {true, 311768064, 33.932052831182766},
            {true, 491211752, 60.8721763669697},
            {true, 325078653, 99.28028490687169},
            {true, 126963254, 71.19160065459147},
            {true, 298087434, 82.9604774178826},
            {false, 0, 0.0f},
            {true, 33475664, 22.22124535022324},
            {true, 372169970, 74.00631733651011},
            {true, 971336108, 83.14767201131882},
            {true, 571803246, 11.428300451405573},
            {true, 429952931, 77.0863175102885},
            {true, 723017655, 68.16674808497214},
            {true, 616912304, 80.52651149979516},
            {false, 0, 0.0f},
            {true, 880809399, 4.258850830859832},
            {false, 0, 0.0f},
            {true, 52400672, 43.29865002745877},
            {true, 516106192, 25.668706393029836},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {true, 774986094, 86.1018815033307},
            {true, 360287973, 49.49569175577015},
            {true, 281939049, 39.99459246920577},
            {true, 892868611, 3.2031345299690837},
            {true, 473907923, 35.68647742645601},
            {true, 860812946, 71.72897000847412},
            {true, 552199709, 89.05936017145255},
            {true, 388388142, 7.775381070719746},
            {true, 885936333, 19.73893386933208},
            {false, 0, 0.0f},
            {true, 945678556, 92.1804914175329},
            {true, 938211316, 51.81772373000226},
            {true, 618728751, 12.589892778947291},
            {false, 0, 0.0f},
            {true, 608056883, 54.000398892571724},
            {true, 797461199, 52.29095461736943},
            {true, 917577583, 72.79425825848472},
            {true, 333101546, 9.883659339000435},
            {true, 965002092, 4.018650431478658},
            {true, 697208154, 90.64275829113174},
            {true, 585181732, 11.053402340774738},
            {true, 620345361, 99.2275038974684},
            {true, 121161793, 5.532736808901487},
            {true, 12967982, 50.05521352159485},
            {true, 669178705, 7.449067878495677},
            {true, 888993748, 30.700911291441614},
            {false, 0, 0.0f},
            {true, 812419646, 65.06138397286047},
            {true, 632660564, 53.402060240218695},
            {true, 657176062, 44.64627235824779},
            {true, 166542499, 72.96850750262128},
            {true, 474207958, 18.109559622205236},
            {false, 0, 0.0f},
            {true, 632379806, 59.867048390028636},
            {true, 893656797, 80.0382199939544},
            {true, 615634056, 50.56535231445677},
            {true, 551342756, 85.49371083096156},
            {true, 916775901, 12.488498364883696},
            {true, 807087172, 13.004933910134197},
            {true, 247875267, 94.33412960710277},
            {true, 595450484, 22.518474339334933},
            {true, 375735286, 69.14715400557094},
            {true, 147176399, 14.862978475352351},
            {true, 647235293, 21.295175348878814},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {true, 918130805, 99.5206051732044},
            {false, 0, 0.0f},
            {true, 358327371, 6.19636283610596},
            {false, 0, 0.0f},
            {true, 484400385, 90.13967976618525},
            {true, 592041604, 31.931922984300698},
            {false, 0, 0.0f},
            {false, 0, 0.0f},
            {true, 131163117, 83.79433086682539},
            {false, 0, 0.0f},
            {true, 934278040, 62.39161192063027},
            {true, 298994981, 70.78561403688526},
            {false, 0, 0.0f},
            {true, 105368048, 99.40775626905061},
            {true, 621918159, 63.87512615564709},
            {true, 718114096, 46.97425718499749},
            {true, 199520477, 99.87551579040812},
            {true, 575745191, 54.551990390607216},
            {true, 541273913, 12.57155760395759},
            {true, 351323069, 41.30129651503819},
            {true, 218879304, 29.293405579644162},
            {true, 813470617, 77.97882158787351},
            {true, 910230605, 85.04624084604544},
            {true, 727344161, 22.802761987139217},
            {true, 155567533, 78.58022352007488},
            {false, 0, 0.0f},
            {true, 58288005, 35.54428757545565},
            {true, 376120208, 88.71887581633028},
            {true, 68357654, 2.3508812712750604},
            {true, 813192805, 27.982502214630777},
            {true, 607180436, 16.604026320325296},
            {true, 684023313, 62.574563960849524},
            {true, 612136167, 84.47927633093843},
            {true, 246360826, 75.3173573522723},
            {true, 391840417, 75.6411841277213},
            {true, 866640231, 59.30719311478474},
            {false, 0, 0.0f},
            {true, 721528691, 21.83257897155658},
            {true, 732895105, 66.42138031006863},
            {true, 48632785, 74.98457530307003},
            {true, 932788001, 84.57250869911344},
            {false, 0, 0.0f},
            {true, 184844282, 23.5234903730176},
            {true, 847453574, 95.96207858720149},
            {true, 621605685, 79.30698657643822},
            {false, 0, 0.0f},
            {true, 591846021, 67.8541830290874},
            {true, 154121936, 32.517672196522554},
            {true, 585803736, 58.22113530988516},
            {true, 336380994, 80.32309483446446},
            {true, 61788203, 58.43596840675908},
            {true, 99950288, 56.060939268453815},
            {true, 464246856, 23.86125377607018},
            {true, 62349427, 23.130488593969982},
            {true, 244304927, 65.2072363214324},
            {false, 0, 0.0f},
            {true, 74583073, 38.3167242044932},
            {true, 425755650, 14.120066222804123},
            {true, 895772919, 70.73947087929773},
            {true, 254317383, 82.58438480876731},
            {true, 828103278, 9.572225639387643},
            {true, 646776682, 68.18423837768886},
            {true, 428392224, 47.363134458470036},
            {false, 0, 0.0f},
            {true, 946493044, 56.93927884639809},
            {true, 848715073, 61.166526074781515},
            {true, 321790554, 48.41492282288697},
            {true, 131246439, 12.453650217470372},
            {true, 106820871, 59.69634962697298},
            {true, 559855813, 68.49886168111006},
            {true, 412394132, 10.156584161718932},
            {true, 77797029, 54.84962483545263},
            {true, 730561182, 17.76940249085081},
            {true, 657017712, 75.8136092298135},
            {true, 675600134, 72.31674342490885},
            {true, 722570197, 35.41388785242076},
            {false, 0, 0.0f},
            {true, 313319047, 68.01327788019861},
            {true, 296462873, 15.872948224213413},
            {true, 820556435, 15.019348822046947},
            {true, 305925781, 39.14654649450848},
            {true, 748951594, 19.784537848237164},
            {true, 943108333, 49.42918680801723},
            {false, 0, 0.0f},
            {true, 748560569, 56.674101234205914},
            {true, 712485315, 29.99890154577189},
            {true, 596708024, 76.95007715131027},
            {true, 7495410, 83.69676338698156},
            {true, 791465545, 83.78339875087184},
            {true, 347319713, 82.33423648573361},
            {true, 743820677, 63.655571529719346},
            {true, 449159274, 15.004561591098124},
            {true, 143848847, 3.76136153624694},
            {true, 442619469, 87.89209016944395},
            {true, 523578072, 32.78405342639873},
            {true, 691059371, 67.80816195830064},
            {true, 215367530, 94.12510553598761},
        };

        cld::IntrVarAllocator<Base> allocator;
        for (std::size_t i = 0; i < 100; i++)
        {
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
