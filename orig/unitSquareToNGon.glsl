
vec3 unitSquareToNGon(vec2 p, float n, float amount)
{
    float a = p.x * 2.0 - 1.0;
    float b = p.y * 2.0 - 1.0;

    float pi = 3.141592;

    float r, theta;
    if (a > -b)
    {
        if (a > b)
        {
            r = a;
            theta = (pi / 4.0) * (b / a);
        }
        else
        {
            r = b;
            theta = (pi / 4.0) * (2.0 - (a / b));
        }
    }
    else
    {
        if (a < b)
        {
            r = -a;
            theta = (pi / 4.0) * (4.0 + (b / a));
        }
        else
        {
            r = -b;
            if (b != 0.0)
            {
                theta = (pi / 4.0) * (6.0 - (a / b));
            }
            else
            {
                theta = 0.0;
            }
        }
    }

    float circleRadius = r;

    r *= mix(1.0, cos(pi / n) / cos(theta - (2.0 * pi / n) * floor((n * theta + pi) / (2.0 * pi))), amount);
    // This is just so that the shape isn't aligned to an axis, which looks a bit nicer
    theta += .6;

    float u = r * cos(theta);
    float v = r * sin(theta);
    return vec3(u, v, circleRadius);
}
