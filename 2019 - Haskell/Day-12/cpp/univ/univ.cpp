
#include <iostream>
#include <array>

using vec3 = std::array<long, 3>;

struct pose
{
    vec3 pos, vel;
};

// test
//std::array<pose, 4> moons =
//{
//    pose{{-1,   0,  2}, {0, 0, 0}},
//    pose{{ 2, -10, -7}, {0, 0, 0}},
//    pose{{ 4,  -8,  8}, {0, 0, 0}},
//    pose{{ 3,   5, -1}, {0, 0, 0}}
//};

std::array<pose, 4> moons =
{
    pose{{ 1,   4,  4}, {0, 0, 0}},
    pose{{ -4, -1, 19}, {0, 0, 0}},
    pose{{-15,-14, 12}, {0, 0, 0}},
    pose{{-17,  1, 10}, {0, 0, 0}}
};

std::array<pose, 4> state = moons;

inline void calcPositions()
{
    for (int i = 0; i < 4; ++i)
        for (int j = 0; j < 3; j++)
            moons[i].pos[j] += moons[i].vel[j];
}

#define SIGN(x) ((x > 0) ? 1 : ((x < 0) ? -1 : 0))
#define ZERO_V(v) (v[0] = v[1] = v[2] = 0)
#define CMP_V(v1, v2) (v1[0] == v2[0] && v1[1] == v2[1] && v1[2] == v2[2])
#define CMP_P(p1, p2) (CMP_V(p1.pos, p2.pos) && CMP_V(p1.vel, p2.vel))
#define CMP_M(m1, m2) (CMP_P(m1[0], m2[0]) && CMP_P(m1[1], m2[1]) && CMP_P(m1[2], m2[2]) && CMP_P(m1[3], m2[3]))

inline void calcVelocities()
{
    for (int i = 0; i < 4; ++i)
    {
        //ZERO_V(moons[i].vel);
        for (int j = 0; j < 4; j++)
        {
            if (i != j)
            {
                for (int k = 0; k < 3; k++)
                    moons[i].vel[k] += SIGN(moons[j].pos[k] - moons[i].pos[k]);
            }
        }
    }
}

struct pose1d
{
    long pos, vel = 0;
};

using dim = std::array<pose1d, 4>;

std::array<dim, 3> moon1d
{
    dim {pose1d{moons[0].pos[0]}, pose1d{moons[1].pos[0]}, pose1d{moons[2].pos[0]}, pose1d{moons[3].pos[0]}},
    dim {pose1d{moons[0].pos[1]}, pose1d{moons[1].pos[1]}, pose1d{moons[2].pos[1]}, pose1d{moons[3].pos[1]}},
    dim {pose1d{moons[0].pos[2]}, pose1d{moons[1].pos[2]}, pose1d{moons[2].pos[2]}, pose1d{moons[3].pos[2]}},
};

auto start1d = moon1d;

#define CALC_VEL(d, i, j) (SIGN(d[j].pos - d[i].pos))

#define CMP_P1D(p1, p2) (p1.pos == p2.pos && p1.vel == p2.vel)
#define CMP_M1D(m1, m2) (CMP_P1D(m1[0], m2[0]) && CMP_P1D(m1[1], m2[1]) && CMP_P1D(m1[2], m2[2]) && CMP_P1D(m1[3], m2[3]))

int main()
{
    for (size_t ii = 0; ii < 3; ++ii)
    {
        auto& dimo = moon1d[ii];
        auto& dims = start1d[ii];
        size_t count = 0;
        do
        {
            dimo[0].vel += (CALC_VEL(dimo, 0, 1) + CALC_VEL(dimo, 0, 2) + CALC_VEL(dimo, 0, 3));
            dimo[1].vel += (CALC_VEL(dimo, 1, 0) + CALC_VEL(dimo, 1, 2) + CALC_VEL(dimo, 1, 3));
            dimo[2].vel += (CALC_VEL(dimo, 2, 0) + CALC_VEL(dimo, 2, 1) + CALC_VEL(dimo, 2, 3));
            dimo[3].vel += (CALC_VEL(dimo, 3, 0) + CALC_VEL(dimo, 3, 1) + CALC_VEL(dimo, 3, 2));

            dimo[0].pos += dimo[0].vel;
            dimo[1].pos += dimo[1].vel;
            dimo[2].pos += dimo[2].vel;
            dimo[3].pos += dimo[3].vel;

            ++count;
        } while (!CMP_M1D(dimo, dims));
        std::cout << count << std::endl;
    }
    // size_t count = 0;
    // do 
    // {
        // calcVelocities();
        // calcPositions();
        // ++count;
    // } while (!CMP_M(moons, state));
    // std::cout << count;
}
