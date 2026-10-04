(function (root, factory) {
  if (typeof module === 'object' && module.exports) {
    module.exports = factory();
  } else {
    root.partsSorterImages = factory();
  }
}(typeof self !== 'undefined' ? self : this, function () {
  const DEFAULT_DEADLINE_MS = 30000;

  function defaultDeps() {
    return {
      fetch: (url, opts) => fetch(url, opts),
      sleep: ms => new Promise(resolve => setTimeout(resolve, ms)),
      now: () => Date.now(),
      createObjectURL: blob => URL.createObjectURL(blob),
      deadlineMs: DEFAULT_DEADLINE_MS
    };
  }

  function retryAfterMs(response) {
    const seconds = parseInt(response.headers.get('Retry-After'), 10);
    if (isNaN(seconds)) return 2000;
    return Math.min(seconds * 1000, 5000);
  }

  function hideImageCell(cell) {
    if (cell) cell.style.visibility = 'hidden';
  }

  async function loadImage(img, deps = defaultDeps()) {
    const ldrawUrl = img.dataset.imageLdraw;
    const bricksetUrl = img.dataset.imageBrickset;
    const cell = img.closest('td');
    const deadline = deps.now() + deps.deadlineMs;

    try {
      let response = await deps.fetch(ldrawUrl);
      while (response.status === 503 && deps.now() < deadline) {
        await deps.sleep(retryAfterMs(response));
        response = await deps.fetch(ldrawUrl);
      }
      if (response.ok) {
        img.src = deps.createObjectURL(await response.blob());
        return;
      }
      if (response.status !== 404 || !bricksetUrl) {
        hideImageCell(cell);
        return;
      }

      response = await deps.fetch(bricksetUrl, { redirect: 'manual' });
      while (response.status === 503 && deps.now() < deadline) {
        await deps.sleep(retryAfterMs(response));
        response = await deps.fetch(bricksetUrl, { redirect: 'manual' });
      }
      if (response.type === 'opaqueredirect') {
        img.src = bricksetUrl;
        return;
      }
      if (response.ok) {
        img.src = deps.createObjectURL(await response.blob());
        return;
      }
      hideImageCell(cell);
    } catch (err) {
      hideImageCell(cell);
    }
  }

  function loadLazyImages(deps = defaultDeps()) {
    document.querySelectorAll('img[data-image-ldraw]').forEach(img => {
      loadImage(img, deps);
    });
  }

  return {
    loadLazyImages,
    loadImage,
    retryAfterMs,
    hideImageCell
  };
}));
