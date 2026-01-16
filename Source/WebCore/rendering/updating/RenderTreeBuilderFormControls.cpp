/*
 * Copyright (C) 2017-2026 Apple Inc. All rights reserved.
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
 * THIS SOFTWARE IS PROVIDED BY APPLE INC. AND ITS CONTRIBUTORS ``AS IS''
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
 * THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL APPLE INC. OR ITS CONTRIBUTORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
 * THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "config.h"
#include "RenderTreeBuilderFormControls.h"

#include "ContainerNodeInlines.h"
#include "HTMLInputElement.h"
#include "HTMLOptionElement.h"
#include "HTMLSelectElement.h"
#include "InputType.h"
#include "RenderBlockFlow.h"
#include "RenderBlockInlines.h"
#include "RenderButton.h"
#include "RenderDescendantIterator.h"
#include "RenderTreeBuilderBlock.h"
#include "RenderTreeUpdaterGeneratedContent.h"
#include "Settings.h"
#include <wtf/TZoneMallocInlines.h>

namespace WebCore {

WTF_MAKE_TZONE_ALLOCATED_IMPL(RenderTreeBuilder::FormControls);

RenderTreeBuilder::FormControls::FormControls(RenderTreeBuilder& builder)
    : m_builder(builder)
{
}

void RenderTreeBuilder::FormControls::attach(RenderButton& parent, RenderPtr<RenderObject> child, RenderObject* beforeChild)
{
    m_builder.blockBuilder().attach(findOrCreateParentForChild(parent), WTF::move(child), beforeChild);
}

RenderPtr<RenderObject> RenderTreeBuilder::FormControls::detach(RenderButton& parent, RenderObject& child, RenderTreeBuilder::WillBeDestroyed willBeDestroyed)
{
    auto* innerRenderer = parent.innerRenderer();
    if (!innerRenderer || &child == innerRenderer || child.parent() == &parent) {
        ASSERT(&child == innerRenderer || !innerRenderer);
        return m_builder.blockBuilder().detach(parent, child, willBeDestroyed);
    }
    return m_builder.detach(*innerRenderer, child, willBeDestroyed);
}


RenderBlock& RenderTreeBuilder::FormControls::findOrCreateParentForChild(RenderButton& parent)
{
    auto* innerRenderer = parent.innerRenderer();
    if (innerRenderer)
        return *innerRenderer;

    auto wrapper = Block::createAnonymousBlockWithStyle(protect(parent.document()), parent.style());
    innerRenderer = wrapper.get();
    m_builder.blockBuilder().attach(parent, WTF::move(wrapper), nullptr);
    parent.setInnerRenderer(*innerRenderer);
    return *innerRenderer;
}

void RenderTreeBuilder::FormControls::updateAfterDescendants(RenderElement& renderer)
{
    if (!renderer.document().settings().cssAppearanceBaseEnabled())
        return;

    if (RefPtr inputElement = dynamicDowncast<HTMLInputElement>(renderer.element())) {
        RefPtr inputType = inputElement->inputType();
        if (!inputType)
            return;

        if (inputType->isCheckable())
            updateCheckmark(renderer);
    }

    if (RefPtr optionElement = dynamicDowncast<HTMLOptionElement>(renderer.element()))
        updateCheckmark(renderer);
}

void RenderTreeBuilder::FormControls::updateCheckmark(RenderElement& renderer)
{
    auto pseudoStyle = renderer.getCachedPseudoStyle({ PseudoElementType::Checkmark });
    if (!pseudoStyle)
        return;

    auto shouldHaveCheckmarkRenderer = [&]() -> bool {
        if (pseudoStyle->display() == DisplayType::None)
            return false;

        CheckedPtr baseRenderer = [&]() -> RenderElement* {
            if (RefPtr optionElement = dynamicDowncast<HTMLOptionElement>(renderer.element())) {
                if (RefPtr selectElement = optionElement->ownerSelectElement())
                    return selectElement->renderer();
            }
            ASSERT(is<HTMLInputElement>(renderer.element()));
            return &renderer;
        }();
        return baseRenderer && baseRenderer->style().usedAppearance() == StyleAppearance::Base;
    };

    auto existingCheckmark = [&]() -> CheckedPtr<RenderBlockFlow> {
        for (CheckedRef child : childrenOfType<RenderElement>(renderer)) {
            if (child->style().pseudoElementType() == PseudoElementType::Checkmark)
                return dynamicDowncast<RenderBlockFlow>(child);
        }
        return nullptr;
    }();

    if (!shouldHaveCheckmarkRenderer()) {
        if (existingCheckmark)
            m_builder.destroy(*existingCheckmark);
        return;
    }

    if (existingCheckmark && existingCheckmark->style().content() == pseudoStyle->content()) {
        auto checkmarkStyle = RenderStyle::clone(*pseudoStyle);
        existingCheckmark->setStyle(WTF::move(checkmarkStyle));
        RenderTreeUpdater::GeneratedContent::updateStyleForContentRenderers(*existingCheckmark, existingCheckmark->style());
        return;
    }

    if (existingCheckmark) {
        m_builder.destroy(*existingCheckmark);
        existingCheckmark = nullptr;
    }

    Ref document = renderer.document();
    auto checkmarkStyle = RenderStyle::clone(*pseudoStyle);

    RenderPtr<RenderBlockFlow> checkmark = createRenderer<RenderBlockFlow>(RenderObject::Type::BlockFlow, document, WTF::move(checkmarkStyle));
    checkmark->initializeStyle();

    if (checkmark->style().hasContent())
        RenderTreeUpdater::GeneratedContent::createContentRenderers(m_builder, *checkmark, checkmark->style(), PseudoElementType::Checkmark);

    m_builder.attach(renderer, WTF::move(checkmark), is<HTMLOptionElement>(renderer.element()) ? renderer.firstChild() : nullptr);
}

} // namespace WebCore
