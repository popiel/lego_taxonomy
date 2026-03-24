const {
  COLUMN_IDS,
  CATEGORY_COLUMNS,
  getCategoryRange,
  moveToDropPosition,
  resetColumns
} = require('../../main/resources/columnOrder');

describe('ColumnOrder', () => {
  describe('COLUMN_IDS', () => {
    it('should have 9 columns in correct order', () => {
      expect(COLUMN_IDS).toHaveLength(9);
      expect(COLUMN_IDS[0]).toBe('category');
      expect(COLUMN_IDS[4]).toBe('image');
      expect(COLUMN_IDS[8]).toBe('partNumber');
    });

    it('should have category columns in first 4 positions', () => {
      expect(COLUMN_IDS.slice(0, 4)).toEqual(['category', 'category2', 'category3', 'category4']);
    });
  });

  describe('CATEGORY_COLUMNS', () => {
    it('should have 4 category columns', () => {
      expect(CATEGORY_COLUMNS).toHaveLength(4);
    });
  });

  describe('getCategoryRange', () => {
    it('should return range for default column order', () => {
      const range = getCategoryRange(COLUMN_IDS);
      expect(range.start).toBe(0);
      expect(range.end).toBe(3);
    });

    it('should return range when categories are in middle', () => {
      const order = ['image', 'name', 'category', 'category2', 'category3', 'category4', 'color', 'quantity', 'partNumber'];
      const range = getCategoryRange(order);
      expect(range.start).toBe(2);
      expect(range.end).toBe(5);
    });

    it('should return range when categories are at end', () => {
      const order = ['image', 'color', 'quantity', 'name', 'partNumber', 'category', 'category2', 'category3', 'category4'];
      const range = getCategoryRange(order);
      expect(range.start).toBe(5);
      expect(range.end).toBe(8);
    });

    it('should return -1,-1 when no categories', () => {
      const order = ['image', 'color', 'quantity', 'name', 'partNumber'];
      const range = getCategoryRange(order);
      expect(range.start).toBe(-1);
      expect(range.end).toBe(-1);
    });
  });

  describe('moveToDropPosition', () => {
    it('should move column from earlier to later position', () => {
      const result = moveToDropPosition(COLUMN_IDS, 0, false, 8);
      expect(result[0]).toBe('category2');
      expect(result[7]).toBe('category');
      expect(result[8]).toBe('partNumber');
    });

    it('should move column from later to earlier position', () => {
      const result = moveToDropPosition(COLUMN_IDS, 8, false, 0);
      expect(result[0]).toBe('partNumber');
      expect(result[1]).toBe('category');
      expect(result[8]).toBe('name');
    });

    it('should move column to adjacent position when within category range', () => {
      // Moving column 0 to position 3 (adjacent to category range end)
      // Position 3 is within category range, effectivePos = 5, insertIndex = 4
      // After removal: ['category2', ...], inserting at 4: [..., 'category']
      const result = moveToDropPosition(COLUMN_IDS, 0, false, 3);
      expect(result).toEqual(['category2', 'category3', 'category4', 'category', 'image', 'color', 'quantity', 'name', 'partNumber']);
    });

    it('should not modify original array', () => {
      const copy = [...COLUMN_IDS];
      moveToDropPosition(COLUMN_IDS, 0, false, 8);
      expect(COLUMN_IDS).toEqual(copy);
    });

    it('should return unchanged when no move needed', () => {
      const result = moveToDropPosition(COLUMN_IDS, 4, false, 4);
      expect(result).toEqual(COLUMN_IDS);
    });

    describe('moving categories', () => {
      it('should move all categories together', () => {
        const result = moveToDropPosition(COLUMN_IDS, 1, true, 6);
        const categoryColumnsInResult = result.filter(col => CATEGORY_COLUMNS.includes(col));
        expect(categoryColumnsInResult).toHaveLength(4);
      });

      it('should move categories to after quantity', () => {
        const result = moveToDropPosition(COLUMN_IDS, 0, true, 6);
        const categoryColumnsInResult = result.filter(col => CATEGORY_COLUMNS.includes(col));
        expect(categoryColumnsInResult).toHaveLength(4);
      });

      it('should not allow repositioning within category range', () => {
        const result = moveToDropPosition(COLUMN_IDS, 1, true, 2);
        expect(result).toEqual(COLUMN_IDS);
      });

      it('should move categories to end', () => {
        const result = moveToDropPosition(COLUMN_IDS, 0, true, 9);
        const categoryIndices = result
          .map((col, idx) => CATEGORY_COLUMNS.includes(col) ? idx : -1)
          .filter(idx => idx >= 0);
        expect(categoryIndices[categoryIndices.length - 1]).toBe(8);
      });
    });

    describe('preventing non-category columns between categories', () => {
      it('should keep categories contiguous after repositioning', () => {
        const order = ['image', 'color', 'category', 'category2', 'category3', 'category4', 'quantity', 'name', 'partNumber'];
        // Dropping quantity (index 6) at position 4 (within category range)
        // Categories at [2,3,4,5], range.end = 5
        // effectivePos = 6, insertIndex = 6 - 1 = 5
        const result = moveToDropPosition(order, 6, false, 4);
        const categoryIndices = result
          .map((col, idx) => CATEGORY_COLUMNS.includes(col) ? idx : -1)
          .filter(idx => idx >= 0);
        // Categories remain contiguous
        expect(categoryIndices[categoryIndices.length - 1] - categoryIndices[0]).toBe(categoryIndices.length - 1);
      });
    });
  });

  describe('resetColumns', () => {
    it('should return columns to default order', () => {
      const reordered = ['image', 'color', 'category', 'category2', 'category3', 'category4', 'quantity', 'name', 'partNumber'];
      const result = resetColumns();
      expect(result).toEqual(COLUMN_IDS);
      expect(result).not.toBe(COLUMN_IDS);
    });

    it('should return a new array, not the original', () => {
      const result = resetColumns();
      result[0] = 'modified';
      expect(COLUMN_IDS[0]).toBe('category');
    });
  });

  describe('integration scenarios', () => {
    it('should handle dragging color to beginning of table', () => {
      // Color is at index 5, dragging to beginning (before category columns)
      const result = moveToDropPosition(COLUMN_IDS, 5, false, 0);
      // Should move color to position 0
      expect(result[0]).toBe('color');
    });

    describe('dragging category columns', () => {
      it('should move all category columns together when dragging to after quantity', () => {
        const result = moveToDropPosition(COLUMN_IDS, 1, true, 6);
        const categoryIndices = result
          .map((col, idx) => CATEGORY_COLUMNS.includes(col) ? idx : -1)
          .filter(idx => idx >= 0);
        expect(categoryIndices).toEqual([2, 3, 4, 5]);
      });

      it('should not allow repositioning within category range', () => {
        const result = moveToDropPosition(COLUMN_IDS, 1, true, 2);
        expect(result).toEqual(COLUMN_IDS);
      });

      it('should handle dragging category to between color and quantity', () => {
        const result = moveToDropPosition(COLUMN_IDS, 1, true, 6);
        expect(result.indexOf('category')).toBe(2);
        expect(result.indexOf('quantity')).toBe(6);
      });

      it('should allow dragging categories back after moving to end', () => {
        // First move categories to end
        const atEnd = moveToDropPosition(COLUMN_IDS, 1, true, 9);
        // Categories should be at positions 5-8
        expect(atEnd.slice(5, 9)).toEqual(['category', 'category2', 'category3', 'category4']);
        
        // Now try to move them back to beginning
        // When categories are at end, dragging should allow dropping at position 0
        const backToStart = moveToDropPosition(atEnd, 5, true, 0);
        expect(backToStart.slice(0, 4)).toEqual(['category', 'category2', 'category3', 'category4']);
      });

      it('should move categories from middle of table to beginning', () => {
        // Categories at indices 2-5, between name and color
        const order = ['image', 'name', 'category', 'category2', 'category3', 'category4', 'color', 'quantity', 'partNumber'];
        // Drag to position 1 (between image and name)
        const result = moveToDropPosition(order, 3, true, 1);
        // Categories should move to indices 1-4
        expect(result.indexOf('category')).toBe(1);
        expect(result.indexOf('category4')).toBe(4);
      });
    });

    it('should handle dragging name column to between category columns', () => {
      const result = moveToDropPosition(COLUMN_IDS, 7, false, 1);
      // Position 1 is in category range, effectivePos = 5, insertIndex = 4
      // Result: categories at 0-3, name at 4
      expect(result.slice(0, 4)).toEqual(['category', 'category2', 'category3', 'category4']);
      expect(result[4]).toBe('name');
    });

    it('should handle dragging quantity to before categories', () => {
      const result = moveToDropPosition(COLUMN_IDS, 6, false, 0);
      expect(result[0]).toBe('quantity');
      expect(result.slice(1, 5)).toEqual(['category', 'category2', 'category3', 'category4']);
    });

    it('should not allow dragging non-category into middle of categories after repositioning', () => {
      const order = ['image', 'color', 'category', 'category2', 'category3', 'category4', 'quantity', 'name', 'partNumber'];
      // Categories at indices 2-5, dropping at position 4 (within category range)
      // effectivePos = 6, insertIndex = 6
      // After removal: 'name' shifts to index 6, insert 'quantity' at 6, 'name' shifts to 7
      const result = moveToDropPosition(order, 6, false, 4);
      expect(result.indexOf('quantity')).toBe(6);
      const categoryIndices = result
        .map((col, idx) => CATEGORY_COLUMNS.includes(col) ? idx : -1)
        .filter(idx => idx >= 0);
      expect(categoryIndices[categoryIndices.length - 1] - categoryIndices[0]).toBe(categoryIndices.length - 1);
    });
  });
});
