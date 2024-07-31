/*
 * Copyright (C) 2003-2020 Apple Inc. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY APPLE INC. ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL APPLE INC. OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
 * OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
 */

#include "config.h"
#include "ColorBlending.h"

#include "AnimationUtilities.h"
#include "Color.h"
#include "ColorConversion.h"
#include "ColorInterpolation.h"

namespace WebCore {


// normal | multiply | screen | overlay | darken | lighten |
// color-dodge | color-burn | hard-light | soft-light | difference | exclusion
static float blendSeparableChannel(float backdrop, float source, BlendMode blendMode)
{
    switch (blendMode) {
    case BlendMode::Normal:
        return source;
    case BlendMode::Multiply:
        return backdrop * source;
    case BlendMode::Screen:
        return backdrop + source - (backdrop * source);
    case BlendMode::Overlay:
        return blendSeparableChannel(source, backdrop, BlendMode::HardLight);
    case BlendMode::Darken:
        return std::min(backdrop, source);
    case BlendMode::Lighten:
        return std::max(backdrop, source);
    // FIXME: color-dodge and color-burn are swapped in the spec.
    case BlendMode::ColorBurn:
        if (backdrop == 0)
            return 0;
        if (source == 1)
            return 1;
        return std::min(1.f, backdrop / (1 - source));
    case BlendMode::ColorDodge:
        if (backdrop == 1)
            return 1;
        if (source == 0)
            return 0;
        return 1 - std::min(1.f, (1 - backdrop) / source);
    case BlendMode::HardLight:
        if (source < 0.5)
            return blendSeparableChannel(backdrop, 2 * source, BlendMode::Multiply);
        return blendSeparableChannel(backdrop, 2 * source - 1, BlendMode::Screen);
    case BlendMode::SoftLight: {
        auto d = [&]() -> uint8_t {
            if (backdrop <= 0.25)
                return ((16 * backdrop - 12) * backdrop + 4) * backdrop;
            return std::sqrt(backdrop);
        }();
        if (source <= 0.5)
            return backdrop - (1 - 2 * source) * backdrop * (1 - backdrop);
        return backdrop + (2 * source - 1) * (d - backdrop);
    }
    case BlendMode::Difference:
        return std::abs(backdrop - source);
    case BlendMode::Exclusion:
        return backdrop + source - 2 * backdrop * source;       
    default:
        ASSERT_NOT_REACHED();
        return source;
    }
}

// hue | saturation | color | luminosity
static Color blendWithNonSeparableBlendMode(const Color& backdrop, const Color& source, BlendMode blendMode)
{
    auto backdropHSLA = backdrop.toColorTypeLossy<HSLA<float>>().resolved();
    auto sourceHSLA = source.toColorTypeLossy<HSLA<float>>().resolved();

    switch (blendMode) {
    case BlendMode::Hue: {
        auto blendedHSLA = source.toColorTypeLossy<HSLA<float>>().resolved();
        blendedHSLA.saturation = backdropHSLA.saturation;
        blendedHSLA.lightness = backdropHSLA.lightness;
        blendedHSLA.alpha = sourceHSLA.alpha;
        return blendedHSLA;
    }
    case BlendMode::Saturation: {
        auto blendedHSLA = backdrop.toColorTypeLossy<HSLA<float>>().resolved();
        blendedHSLA.saturation = sourceHSLA.saturation;
        blendedHSLA.lightness = backdropHSLA.lightness;
        blendedHSLA.alpha = sourceHSLA.alpha;
        return blendedHSLA;
    }
    case BlendMode::Color: {
        auto blendedHSLA = source.toColorTypeLossy<HSLA<float>>().resolved();
        blendedHSLA.lightness = backdropHSLA.lightness;
        blendedHSLA.alpha = sourceHSLA.alpha;
        return blendedHSLA;
    }
    case BlendMode::Luminosity: {
        auto blendedHSLA = backdrop.toColorTypeLossy<HSLA<float>>().resolved();
        blendedHSLA.lightness = sourceHSLA.lightness;
        blendedHSLA.alpha = sourceHSLA.alpha;
        return blendedHSLA;
    }
    default:
        ASSERT_NOT_REACHED();
        return sourceHSLA;
    }
}

Color blendSourceOver(const Color& backdrop, const Color& source, BlendMode blendMode)
{
    auto [backdropR, backdropG, backdropB, backdropA] = backdrop.toColorTypeLossy<SRGBA<uint8_t>>().resolved();
    auto [sourceR, sourceG, sourceB, sourceA] = source.toColorTypeLossy<SRGBA<uint8_t>>().resolved();

    auto blended = [&]() -> Color {
        if (blendMode == BlendMode::Hue || blendMode == BlendMode::Saturation || blendMode == BlendMode::Color || blendMode == BlendMode::Luminosity)
            return blendWithNonSeparableBlendMode(backdrop, source, blendMode);

        uint8_t blendedR = blendSeparableChannel(backdropR / 0xFF, sourceR / 0xFF, blendMode) * 0xFF;
        uint8_t blendedG = blendSeparableChannel(backdropG / 0xFF, sourceG / 0xFF, blendMode) * 0xFF;
        uint8_t blendedB = blendSeparableChannel(backdropB / 0xFF, sourceB / 0xFF, blendMode) * 0xFF;

        return makeFromComponentsClamping<SRGBA<uint8_t>>(blendedR, blendedG, blendedB, sourceA);
    }();

    auto [blendedR, blendedG, blendedB, blendedA] = blended.toColorTypeLossy<SRGBA<uint8_t>>().resolved();

    if (!backdrop.isVisible() || source.isOpaque())
        return blended;

    if (!source.isVisible())
        return backdrop;

    if (!backdropA || sourceA == 255)
        return blended;

    if (!sourceA)
        return backdrop;

    int d = 0xFF * (backdropA + sourceA) - backdropA * sourceA;
    int a = d / 0xFF;
    int r = (backdropR * backdropA * (0xFF - sourceA) + 0xFF * sourceA * blendedR) / d;
    int g = (backdropG * backdropA * (0xFF - sourceA) + 0xFF * sourceA * blendedG) / d;
    int b = (backdropB * backdropA * (0xFF - sourceA) + 0xFF * sourceA * blendedB) / d;

    return makeFromComponentsClamping<SRGBA<uint8_t>>(r, g, b, a);
}

Color blendWithWhite(const Color& color)
{
    constexpr int startAlpha = 153; // 60%
    constexpr int endAlpha = 204; // 80%;
    constexpr int alphaIncrement = 17;

    auto blendComponent = [](int c, int a) -> int {
        float alpha = a / 255.0f;
        int whiteBlend = 255 - a;
        c -= whiteBlend;
        return static_cast<int>(c / alpha);
    };

    // If the color contains alpha already, we leave it alone.
    if (!color.isOpaque())
        return color;

    auto [existingR, existingG, existingB, existingAlpha] = color.toColorTypeLossy<SRGBA<uint8_t>>().resolved();

    SRGBA<uint8_t> result;
    for (int alpha = startAlpha; alpha <= endAlpha; alpha += alphaIncrement) {
        // We have a solid color.  Convert to an equivalent color that looks the same when blended with white
        // at the current alpha.  Try using less transparency if the numbers end up being negative.
        int r = blendComponent(existingR, alpha);
        int g = blendComponent(existingG, alpha);
        int b = blendComponent(existingB, alpha);

        result = makeFromComponentsClamping<SRGBA<uint8_t>>(r, g, b, alpha);

        if (r >= 0 && g >= 0 && b >= 0)
            break;
    }

    // FIXME: Why is preserving the semantic bit desired and/or correct here?
    if (color.isSemantic())
        return { result, Color::Flags::Semantic };
    return result;
}

static bool requiresLegacyInterpolationRules(const Color& color)
{
    return color.callOnUnderlyingType([&]<typename ColorType> (const ColorType&) {
        if constexpr (std::is_same_v<ColorType, SRGBA<uint8_t>>)
            return true;
        else if constexpr (std::is_same_v<ColorType, SRGBA<float>>)
            return true;
        else if constexpr (std::is_same_v<ColorType, HSLA<float>>)
            return true;
        else if constexpr (std::is_same_v<ColorType, HWBA<float>>)
            return true;
        else if constexpr (std::is_same_v<ColorType, ExtendedSRGBA<float>>)
            return !color.usesColorFunctionSerialization();
        else
            return false;
    });
}

Color blend(const Color& from, const Color& to, const BlendingContext& context)
{
    // We need to preserve the state of the valid flag at the end of the animation
    if (context.progress == 1 && !to.isValid())
        return { };

    if (requiresLegacyInterpolationRules(from) && requiresLegacyInterpolationRules(to)) {
        using InterpolationColorSpace = ColorInterpolationMethod::SRGB;

        auto fromComponents = from.toColorTypeLossyCarryingForwardMissing<typename InterpolationColorSpace::ColorType>();
        auto toComponents = to.toColorTypeLossyCarryingForwardMissing<typename InterpolationColorSpace::ColorType>();

        switch (context.compositeOperation) {
        case CompositeOperation::Replace: {
            auto interpolatedColor = interpolateColorComponents<AlphaPremultiplication::Premultiplied>(InterpolationColorSpace { }, fromComponents, 1.0 - context.progress, toComponents, context.progress);
            return convertColor<SRGBA<uint8_t>>(clipToGamut<SRGBA<float>>(interpolatedColor));
        }
        case CompositeOperation::Add:
        case CompositeOperation::Accumulate:
            ASSERT(context.progress == 1.0);
            return addColorComponents<AlphaPremultiplication::Premultiplied>(InterpolationColorSpace { }, fromComponents, toComponents);
        }
    } else {
        using InterpolationColorSpace = ColorInterpolationMethod::OKLab;

        auto fromComponents = from.toColorTypeLossyCarryingForwardMissing<typename InterpolationColorSpace::ColorType>();
        auto toComponents = to.toColorTypeLossyCarryingForwardMissing<typename InterpolationColorSpace::ColorType>();

        switch (context.compositeOperation) {
        case CompositeOperation::Replace:
            return interpolateColorComponents<AlphaPremultiplication::Premultiplied>(InterpolationColorSpace { }, fromComponents, 1.0 - context.progress, toComponents, context.progress);

        case CompositeOperation::Add:
        case CompositeOperation::Accumulate:
            ASSERT(context.progress == 1.0);
            return addColorComponents<AlphaPremultiplication::Premultiplied>(InterpolationColorSpace { }, fromComponents, toComponents);
        }
    }

    RELEASE_ASSERT_NOT_REACHED();
}

Color blendWithoutPremultiply(const Color& from, const Color& to, const BlendingContext& context)
{
    // FIXME: ExtendedColor - needs to handle color spaces.
    // We need to preserve the state of the valid flag at the end of the animation
    if (context.progress == 1 && !to.isValid())
        return { };

    auto fromSRGB = from.toColorTypeLossy<SRGBA<float>>().resolved();
    auto toSRGB = to.toColorTypeLossy<SRGBA<float>>().resolved();

    auto blended = makeFromComponentsClamping<SRGBA<float>>(
        WebCore::blend(fromSRGB.red, toSRGB.red, context),
        WebCore::blend(fromSRGB.green, toSRGB.green, context),
        WebCore::blend(fromSRGB.blue, toSRGB.blue, context),
        WebCore::blend(fromSRGB.alpha, toSRGB.alpha, context)
    );

    return convertColor<SRGBA<uint8_t>>(blended);
}

} // namespace WebCore
