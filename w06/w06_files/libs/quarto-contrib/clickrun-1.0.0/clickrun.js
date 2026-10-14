// JS-powered outputs (DT tables, plotly, leaflet, ...) that get revealed
// from inside a display:none container can mis-measure themselves at init
// time (DataTables in particular is notorious for computing zero-width
// columns this way). Most htmlwidgets recompute on a window "resize"
// event, so nudge one once the element actually has layout again.
function nudgeWidgets() {
  requestAnimationFrame(() => window.dispatchEvent(new Event("resize")));
}

document.addEventListener("DOMContentLoaded", () => {
  // Quarto's own "Other Formats" auto-linking only skips re-appending a
  // format it already sees a *plain string* entry for in format-links —
  // customizing a format's own label via the {format:, text:} object form
  // (as day00.qmd's front matter does, to relabel Typst's link "PDF")
  // doesn't count, so it gets appended a second time with its default
  // label regardless. Dedupe by href, keeping the first (ours).
  const seenHrefs = new Set();
  document.querySelectorAll(".quarto-alternate-formats li").forEach((li) => {
    const href = li.querySelector("a")?.getAttribute("href");
    if (!href) return;
    if (seenHrefs.has(href)) li.remove();
    else seenHrefs.add(href);
  });

  // Which "Other Formats" links exist, their labels, icons, and order all
  // stay controlled from each day's own format-links YAML — this only
  // prepends this day's actual basename onto any href that's just a bare
  // ".ext" (e.g. ".typ"), so a day's qmd never has to spell out its own
  // filename. format-links itself is resolved before Lua filters run
  // (confirmed empirically — a filter-set value is silently ignored), so
  // this substitution has to happen client-side, not in clickrun.lua.
  // Every format link — the auto-generated one too — opens in a new tab,
  // since following one navigates away from the running click-to-run page.
  const stem = location.pathname.split("/").pop().replace(/\.html$/, "");
  document.querySelectorAll(".quarto-alternate-formats a").forEach((a) => {
    const href = a.getAttribute("href");
    if (href?.startsWith(".")) a.setAttribute("href", stem + href);
    a.setAttribute("target", "_blank");
    a.setAttribute("rel", "noopener");
  });

  // GitHub icon-links for authors — clickrun.lua embeds a small
  // name -> github-username JSON blob, since "github" isn't a field
  // Quarto's own title-block renderer knows to display on its own. Match
  // each entry back up to its author's <p> by name text and append a
  // link next to the (already Quarto-generated) email one.
  const githubDataEl = document.getElementById("clickrun-authors-github");
  if (githubDataEl) {
    const githubData = JSON.parse(githubDataEl.textContent);
    document.querySelectorAll(".quarto-title-meta-contents p").forEach((p) => {
      const entry = githubData.find((a) => p.textContent.includes(a.name));
      if (!entry) return;
      // Quarto leaves a trailing whitespace-only text node after the
      // email link — appending straight after it would stack that
      // whitespace with this link's own CSS margin, reading as too big
      // a gap. Stripping it makes the margin the only spacing in play.
      const last = p.lastChild;
      if (last?.nodeType === Node.TEXT_NODE && !last.textContent.trim()) {
        last.remove();
      }
      const a = document.createElement("a");
      a.href = `https://github.com/${entry.github}`;
      a.className = "quarto-title-author-github";
      a.target = "_blank";
      a.rel = "noopener";
      a.innerHTML = '<i class="fa-brands fa-github"></i>';
      p.appendChild(a);
    });
  }

  // :not(.quarto-float) — numbered-table floats (tbl- labels) reuse the
  // "cell" class on their own wrapper div, which would otherwise get
  // processed a second time as if it were an independent chunk, nested
  // inside the very output group its own button already controls.
  document.querySelectorAll("div.cell:not(.quarto-float)").forEach((cell) => {
    // A single chunk can produce several independent statement "runs" —
    // knitr splits each into its own source scaffold + one or more output
    // divs, but keeps them all as siblings inside one .cell. Group
    // consecutive output elements together (a run can emit more than one,
    // e.g. a warning followed by its result) and give each group its own
    // button, rather than one button for the whole chunk.
    let group = null;

    [...cell.children].forEach((child) => {
      // Numbered tables (tbl- labels) wrap their result in a
      // .cell/.quarto-float div directly under .cell, rather than the
      // usual .cell-output-display — numbered figures don't do this (their
      // .quarto-float sits inside a normal .cell-output-display), but
      // checking for it here covers both without special-casing.
      const isOutput =
        child.classList.contains("cell-output") ||
        child.classList.contains("cell-output-display") ||
        child.classList.contains("quarto-float");

      if (!isOutput) {
        group = null;
        return;
      }

      if (!group) {
        // `group` itself gets reassigned as later statements are found, so
        // the click handler below must close over this specific array
        // (myGroup), not the outer `group` binding — otherwise every
        // button in the cell ends up toggling whichever group was last
        // assigned by the time you click, instead of its own.
        const myGroup = [];
        group = myGroup;

        const btn = document.createElement("button");
        btn.type = "button";
        btn.className = "run-btn";
        btn.textContent = "▶ Run";
        btn.setAttribute("aria-expanded", "false");

        btn.addEventListener("click", () => {
          const open = !myGroup[0].classList.contains("is-open");
          myGroup.forEach((el) => el.classList.toggle("is-open", open));
          btn.textContent = open ? "■ Reset" : "▶ Run";
          btn.setAttribute("aria-expanded", String(open));
          if (open) nudgeWidgets();
        });

        child.insertAdjacentElement("beforebegin", btn);
      }

      group.push(child);
    });
  });

  // "Try it" solution code: `#| class.source: "hide-code"` (knitr chunk
  // option) lands the "hide-code" class on the chunk's <pre>, not its
  // code-copy wrapper div — walk up to that wrapper to toggle the whole
  // source block. A dedicated "Code" button, independent of the output's
  // own Run button, since revealing a written solution is a separate
  // decision from running it.
  document.querySelectorAll("pre.sourceCode.hide-code").forEach((pre) => {
    const scaffold = pre.closest(".code-copy-outer-scaffold");
    if (!scaffold) return;

    const btn = document.createElement("button");
    btn.type = "button";
    btn.className = "run-btn code-btn";
    btn.textContent = "</> Code";
    btn.setAttribute("aria-expanded", "false");

    btn.addEventListener("click", () => {
      const open = !scaffold.classList.contains("is-open");
      scaffold.classList.toggle("is-open", open);
      btn.textContent = open ? "■ Hide Code" : "</> Code";
      btn.setAttribute("aria-expanded", String(open));
    });

    scaffold.insertAdjacentElement("beforebegin", btn);
  });
});

// Delegated: tibble "Table view" / "Console view" toggle (helpers.R's
// knit_print.tbl_df emits a fresh .tibble-view block per tibble, so this
// has to work for elements that didn't exist at DOMContentLoaded time too).
document.addEventListener("click", (e) => {
  const btn = e.target.closest(".view-btn");
  if (!btn) return;
  const wrap = btn.closest(".tibble-view");
  const consoleEl = wrap.querySelector(".tibble-console");
  const tableEl = wrap.querySelector(".tibble-table");
  const showTable = !tableEl.classList.contains("is-open");

  tableEl.classList.toggle("is-open", showTable);
  consoleEl.classList.toggle("is-open", !showTable);
  btn.textContent = showTable ? "▤ Console view" : "▦ Table view";
  btn.setAttribute("aria-expanded", String(showTable));
  if (showTable) nudgeWidgets();
});
