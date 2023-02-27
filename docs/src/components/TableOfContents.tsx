import React, { useState, useCallback, useEffect } from 'react';

type Section = {
  name: string;
  id: string;
  level: number;
  subsections: Section[];
}

function getLevel(heading: HTMLHeadingElement): number {
  return parseInt(heading.tagName.slice(1));
}

function getSection(heading: HTMLHeadingElement) {
  return {
    name: heading.innerText,
    id: heading.id,
    level: getLevel(heading),
    subsections: [],
  };
}


function makeStructure(headings: NodeListOf<HTMLHeadingElement>): Section {
  const headingsQueue = [...headings];

  function makeStructureInner(workingSection: Section): Section {
    // While the next heading is a subsection of this heading
    while (headingsQueue[0] && getLevel(headingsQueue[0]) > workingSection.level) {
      workingSection.subsections.push(
        makeStructureInner(getSection(headingsQueue.shift()!)),
      );
    }
    return workingSection;
  }

  return makeStructureInner({
    name: '(toplevel)',
    id: '(toplevel)',
    level: 0,
    subsections: [],
  });
}


function TOCList({ sections }: { sections: Section[] }) {
  if (!sections.length) return null;

  return (
    <ul className="table-of-contents-list">
      {sections.map((section) => (
        <li key={section.id}>
          <a href={`#${section.id}`}>{section.name}</a>
          <TOCList sections={section.subsections}></TOCList>
        </li>
      ))}
    </ul>
  )
}


export default function TableOfContents({ after = null }: { after?: string | null }) {
  const [structure, setStructure] = useState<Section | null>(null);

  const update = useCallback(() => {
    const headings = document.querySelectorAll(`${after ? `${after} ~ ` : ''} :is(h1, h2, h3, h4, h5, h6)`);
    setStructure(makeStructure(headings as NodeListOf<HTMLHeadingElement>));
  }, []);

  useEffect(() => {
    update();
    const mutationObserver = new MutationObserver(update);
    mutationObserver.observe(document, {
      childList: true,
      subtree: true,
    });
    return () => mutationObserver.disconnect();
  }, []);

  return <TOCList sections={structure?.subsections ?? []} />;
}
