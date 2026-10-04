const {
  loadImage,
  retryAfterMs,
  hideImageCell
} = require('../../main/resources/partsSorterImages');

function fakeImg(ldrawUrl, bricksetUrl) {
  const cell = { style: {} };
  return {
    dataset: { imageLdraw: ldrawUrl, imageBrickset: bricksetUrl },
    src: null,
    closest: selector => (selector === 'td' ? cell : null),
    cell
  };
}

function fakeResponse(overrides = {}) {
  const headers = { get: () => undefined };
  return Object.assign(
    {
      ok: false,
      status: 404,
      type: 'basic',
      headers,
      blob: async () => ({ blob: true })
    },
    overrides
  );
}

function responseWithRetryAfter(seconds) {
  return fakeResponse({
    status: 503,
    headers: { get: name => (name === 'Retry-After' ? String(seconds) : undefined) }
  });
}

function testDeps(handleFetch) {
  const deps = {
    fetchCalls: [],
    sleeps: [],
    blobs: [],
    nowValues: [1000],
    fetch: async (url, options) => {
      deps.fetchCalls.push({ url, options });
      const response = handleFetch(deps.fetchCalls.length, url);
      if (response instanceof Error) throw response;
      return response;
    },
    sleep: async ms => {
      deps.sleeps.push(ms);
    },
    now: () => (deps.nowValues.length > 1 ? deps.nowValues.shift() : deps.nowValues[0]),
    createObjectURL: blob => {
      deps.blobs.push(blob);
      return `blob:object-${deps.blobs.length}`;
    },
    deadlineMs: 30000
  };
  return deps;
}

describe('partsSorterImages', () => {
  describe('loadImage', () => {
    it('shows the LDraw image from a blob URL on 200', async () => {
      const img = fakeImg('/part_images/1/3001.png', '/part_images/brickset/3001');
      const deps = testDeps(() => fakeResponse({ ok: true, status: 200 }));

      await loadImage(img, deps);

      expect(img.src).toBe('blob:object-1');
      expect(deps.blobs).toHaveLength(1);
      expect(img.cell.style.visibility).toBeUndefined();
      expect(deps.fetchCalls).toHaveLength(1);
      expect(deps.fetchCalls[0].url).toBe('/part_images/1/3001.png');
    });

    it('hides the cell when LDraw 404s and no brickset attribute exists', async () => {
      const img = fakeImg('/part_images/1/3001.png', undefined);
      const deps = testDeps(() => fakeResponse({ status: 404 }));

      await loadImage(img, deps);

      expect(img.cell.style.visibility).toBe('hidden');
      expect(img.src).toBeNull();
      expect(deps.fetchCalls).toHaveLength(1);
    });

    it('hides the cell on an unexpected LDraw status without trying Brickset', async () => {
      const img = fakeImg('/part_images/1/3001.png', '/part_images/brickset/3001');
      const deps = testDeps(() => fakeResponse({ status: 500 }));

      await loadImage(img, deps);

      expect(img.cell.style.visibility).toBe('hidden');
      expect(deps.fetchCalls).toHaveLength(1);
    });

    it('sets img.src directly to the brickset URL on opaqueredirect', async () => {
      const img = fakeImg('/part_images/1/3001.png', '/part_images/brickset/3001');
      const deps = testDeps(
        (call, url) =>
          url.includes('/brickset/')
            ? fakeResponse({ status: 0, type: 'opaqueredirect' })
            : fakeResponse({ status: 404 })
      );

      await loadImage(img, deps);

      expect(img.src).toBe('/part_images/brickset/3001');
      expect(deps.blobs).toHaveLength(0);
      expect(img.cell.style.visibility).toBeUndefined();
    });

    it('shows the Brickset image from a blob URL when fetch follows to 200', async () => {
      const img = fakeImg('/part_images/1/3001.png', '/part_images/brickset/3001');
      const deps = testDeps(
        (call, url) =>
          url.includes('/brickset/')
            ? fakeResponse({ ok: true, status: 200 })
            : fakeResponse({ status: 404 })
      );

      await loadImage(img, deps);

      expect(img.src).toBe('blob:object-1');
      expect(img.cell.style.visibility).toBeUndefined();
    });

    it('hides the cell on a definitive Brickset 404', async () => {
      const img = fakeImg('/part_images/1/3001.png', '/part_images/brickset/3001');
      const deps = testDeps(() => fakeResponse({ status: 404 }));

      await loadImage(img, deps);

      expect(img.cell.style.visibility).toBe('hidden');
      expect(img.src).toBeNull();
      expect(deps.fetchCalls).toHaveLength(2);
    });

    it('retries a 503 honoring Retry-After, then shows the image', async () => {
      const img = fakeImg('/part_images/1/3001.png', '/part_images/brickset/3001');
      const deps = testDeps(
        call => (call === 1 ? responseWithRetryAfter(1) : fakeResponse({ ok: true, status: 200 }))
      );

      await loadImage(img, deps);

      expect(img.src).toBe('blob:object-1');
      expect(deps.sleeps).toEqual([1000]);
      expect(deps.fetchCalls).toHaveLength(2);
    });

    it('retries Brickset 503s and then shows the opaqueredirect image', async () => {
      const img = fakeImg('/part_images/1/3001.png', '/part_images/brickset/3001');
      const deps = testDeps((call, url) => {
        if (!url.includes('/brickset/')) return fakeResponse({ status: 404 });
        return call === 2
          ? responseWithRetryAfter(0)
          : fakeResponse({ status: 0, type: 'opaqueredirect' });
      });

      await loadImage(img, deps);

      expect(img.src).toBe('/part_images/brickset/3001');
      expect(deps.sleeps).toEqual([0]);
      expect(deps.fetchCalls).toHaveLength(3);
    });

    it('gives up and hides the cell when 503 persists past the deadline', async () => {
      const img = fakeImg('/part_images/1/3001.png', undefined);
      const deps = testDeps(() => responseWithRetryAfter(0));
      deps.nowValues = [1000, 1000, 31001];

      await loadImage(img, deps);

      expect(img.cell.style.visibility).toBe('hidden');
      expect(img.src).toBeNull();
      expect(deps.fetchCalls).toHaveLength(2);
      expect(deps.sleeps).toEqual([0]);
    });

    it('hides the cell when fetch rejects', async () => {
      const img = fakeImg('/part_images/1/3001.png', '/part_images/brickset/3001');
      const deps = testDeps(() => new Error('network down'));

      await loadImage(img, deps);

      expect(img.cell.style.visibility).toBe('hidden');
      expect(img.src).toBeNull();
    });
  });

  describe('retryAfterMs', () => {
    const responseWithHeader = value =>
      fakeResponse({ headers: { get: () => value } });

    it('defaults to 2000ms without a Retry-After header', () => {
      expect(retryAfterMs(responseWithHeader(undefined))).toBe(2000);
    });

    it('parses seconds into milliseconds', () => {
      expect(retryAfterMs(responseWithHeader('1'))).toBe(1000);
    });

    it('returns 0ms for an immediate retry header', () => {
      expect(retryAfterMs(responseWithHeader('0'))).toBe(0);
    });

    it('caps the delay at 5000ms', () => {
      expect(retryAfterMs(responseWithHeader('10'))).toBe(5000);
    });

    it('falls back to 2000ms for a garbage header', () => {
      expect(retryAfterMs(responseWithHeader('soon'))).toBe(2000);
    });
  });

  describe('hideImageCell', () => {
    it('hides the cell', () => {
      const cell = { style: {} };
      hideImageCell(cell);
      expect(cell.style.visibility).toBe('hidden');
    });

    it('tolerates a missing cell', () => {
      expect(() => hideImageCell(undefined)).not.toThrow();
    });
  });
});
