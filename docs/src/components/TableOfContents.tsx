import React, { useState, useCallback, useEffect } from 'react';

/** Get the numeric level of a heading element (h1–h6) */
function getLevel(heading: HTMLHeadingElement): number {
  return parseInt(heading.tagName.slice(1));
}

type Section = {
  name: string;
  id: string;
  level: number;
  subsections: Section[];
}
/** Get a Section from a HTML heading element */
function parseSection(heading: HTMLHeadingElement) {
  return {
    name: heading.innerText,
    id: heading.id,
    level: getLevel(heading),
    subsections: [],
  };
}


/** Create a Section encapsulating nested structure from a list of HTMLHeadingElements */
function makeStructure(headings: Iterable<HTMLHeadingElement>): Section {
  const headingsQueue = [...headings];

  function makeStructureInner(workingSection: Section): Section {
    // While the next heading is a subsection of this heading
    while (headingsQueue[0] && getLevel(headingsQueue[0]) > workingSection.level) {
      if (getLevel(headingsQueue[0]) > workingSection.level + 1) {
        console.warn(`Heading level skipped between h${workingSection.level}#${workingSection.id} () to h${getLevel(headingsQueue[0])}#${headingsQueue[0].id}`);
      }
      workingSection.subsections.push(
        makeStructureInner(parseSection(headingsQueue.shift()!)),
      );
    }
    return workingSection;
  }

  return makeStructureInner({
    name: '(toplevel)',
    id: '(toplevel)',
    level: getLevel(headingsQueue[0]) - 1,
    subsections: [],
  });
}

/**
 * Return a list of nested Sections
 * @param after A selector to find headings *after*
 */
function useHeadingSections(afterId?: string): Section[] | null {
  const [structure, setStructure] = useState<Section | null>(null);

  const update = useCallback(() => {
    const headings = [...document.querySelectorAll('h1, h2, h3, h4, h5, h6')] as HTMLHeadingElement[];
    if (afterId) {
      const afterIdx = headings.findIndex((heading) => heading.id === afterId);
      console.log(afterId, afterIdx + 1);
      if (afterIdx !== -1) {
        headings.splice(0, afterIdx + 1);
      }
    }
    setStructure(makeStructure(headings));
  }, [afterId]);

  useEffect(() => {
    update();
    const mutationObserver = new MutationObserver(update);
    mutationObserver.observe(document, {
      childList: true,
      subtree: true,
    });
    return () => mutationObserver.disconnect();
  }, [update]);

  return structure?.subsections ?? null;
}


function TOCList({ sections }: { sections: Section[] }) {
  if (!sections.length) return null;

  return (
    <ol className="table-of-contents">
      {sections.map((section) => (
        <li key={section.id}>
          <a href={`#${section.id}`}>{section.name}</a>
          <TOCList sections={section.subsections}></TOCList>
        </li>
      ))}
    </ol>
  )
}


export default function TableOfContents({ afterId }: { afterId?: string }) {
  const sections = useHeadingSections(afterId);
  return <TOCList sections={sections ?? []} />;
}
