const {
  COLUMN_IDS,
  CATEGORY_COLUMNS,
  getColumnIndex,
  isCategoryColumn,
  getValidDropPositions,
  computeDropPositionRanges,
  getDropPositionFromX,
  getDropPositionDisplayInfo,
  getInsertIndex,
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

  describe('getColumnIndex', () => {
    it('should return correct index for each column', () => {
      expect(getColumnIndex(COLUMN_IDS, 'category')).toBe(0);
      expect(getColumnIndex(COLUMN_IDS, 'category2')).toBe(1);
      expect(getColumnIndex(COLUMN_IDS, 'category3')).toBe(2);
      expect(getColumnIndex(COLUMN_IDS, 'category4')).toBe(3);
      expect(getColumnIndex(COLUMN_IDS, 'image')).toBe(4);
      expect(getColumnIndex(COLUMN_IDS, 'color')).toBe(5);
      expect(getColumnIndex(COLUMN_IDS, 'quantity')).toBe(6);
      expect(getColumnIndex(COLUMN_IDS, 'name')).toBe(7);
      expect(getColumnIndex(COLUMN_IDS, 'partNumber')).toBe(8);
    });

    it('should return -1 for non-existent column', () => {
      expect(getColumnIndex(COLUMN_IDS, 'invalid')).toBe(-1);
      expect(getColumnIndex(COLUMN_IDS, 'foo')).toBe(-1);
    });
  });

  describe('isCategoryColumn', () => {
    it('should return true for category columns', () => {
      expect(isCategoryColumn('category')).toBe(true);
      expect(isCategoryColumn('category2')).toBe(true);
      expect(isCategoryColumn('category3')).toBe(true);
      expect(isCategoryColumn('category4')).toBe(true);
    });

    it('should return false for non-category columns', () => {
      expect(isCategoryColumn('image')).toBe(false);
      expect(isCategoryColumn('color')).toBe(false);
      expect(isCategoryColumn('quantity')).toBe(false);
      expect(isCategoryColumn('name')).toBe(false);
      expect(isCategoryColumn('partNumber')).toBe(false);
    });
  });

  describe('getValidDropPositions', () => {
    describe('dragging non-category column', () => {
      it('should exclude adjacent positions when dragging first column', () => {
        const positions = getValidDropPositions(COLUMN_IDS, 0, false);
        // Position 1 is adjacent (after column 0)
        expect(positions).not.toContain(0);
        expect(positions).not.toContain(1);
      });

      it('should include current position when dragging non-category column', () => {
        // Color is at index 5
        const positions = getValidDropPositions(COLUMN_IDS, 5, false);
        // Position 5 should be included as valid (current position)
        expect(positions).toContain(5);
      });

      it('should exclude adjacent and category range positions when dragging first column', () => {
        const positions = getValidDropPositions(COLUMN_IDS, 0, false);
        // Positions 0,1,2,3 are in category range or adjacent
        // Valid: 4,5,6,7,8,9
        expect(positions).toEqual([4, 5, 6, 7, 8, 9]);
      });

      it('should exclude adjacent and category range positions when dragging last column', () => {
        const positions = getValidDropPositions(COLUMN_IDS, 8, false);
        // Adjacent: 7,8,9. Category range: 0,1,2,3
        // Position 8 is current position, so included
        // Valid: 0,4,5,6,7,8
        expect(positions).toEqual([0, 4, 5, 6, 7, 8]);
      });

      it('should exclude adjacent and category range positions when dragging middle column', () => {
        const positions = getValidDropPositions(COLUMN_IDS, 4, false);
        // Adjacent: 3,5. Category range: 0,1,2,3
        // Position 4 is current position, so included
        // Valid: 0,4,6,7,8,9
        expect(positions).toEqual([0, 4, 6, 7, 8, 9]);
      });

      it('should exclude category range positions when dragging non-category', () => {
        const positions = getValidDropPositions(COLUMN_IDS, 6, false);
        // Category range: 0,1,2,3
        // Position 6 is current position, so included
        // Adjacent: 5,7
        // Valid: 0,4,5,6,8,9
        expect(positions).toEqual([0, 4, 5, 6, 8, 9]);
      });
    });

    describe('dragging category column', () => {
      it('should only allow positions outside category range', () => {
        const positions = getValidDropPositions(COLUMN_IDS, 1, true);
        // Categories at 0,1,2,3. Only positions after range valid.
        expect(positions).toEqual([5, 6, 7, 8, 9]);
      });

      it('should treat all categories as one block', () => {
        const positionsDraggingCat1 = getValidDropPositions(COLUMN_IDS, 1, true);
        const positionsDraggingCat3 = getValidDropPositions(COLUMN_IDS, 3, true);
        expect(positionsDraggingCat1).toEqual(positionsDraggingCat3);
      });
    });
  });

  describe('computeDropPositionRanges', () => {
    it('should create non-overlapping ranges covering all x values', () => {
      const boundaries = [0, 100, 200, 300];
      const validPositions = [1, 3, 4];
      const ranges = computeDropPositionRanges(boundaries, validPositions);

      expect(ranges.length).toBe(validPositions.length);
      expect(ranges[0].x1).toBe(-Infinity);
      expect(ranges[0].dropPosition).toBe(1);
      expect(ranges[ranges.length - 1].x2).toBe(Infinity);
      expect(ranges[ranges.length - 1].dropPosition).toBe(4);

      for (let i = 0; i < ranges.length - 1; i++) {
        expect(ranges[i].x2).toBe(ranges[i + 1].x1);
      }
    });

    it('should map x values to correct drop positions', () => {
      const boundaries = [0, 100, 200, 300];
      const validPositions = [1, 2, 3];
      const ranges = computeDropPositionRanges(boundaries, validPositions);

      expect(getDropPositionFromX(ranges, 50)).toBe(1);
      expect(getDropPositionFromX(ranges, 150)).toBe(2);
      expect(getDropPositionFromX(ranges, 250)).toBe(3);
    });

    it('should handle x values before first boundary', () => {
      const boundaries = [100, 200, 300];
      const validPositions = [0, 2, 3];
      const ranges = computeDropPositionRanges(boundaries, validPositions);

      expect(getDropPositionFromX(ranges, -100)).toBe(0);
      expect(getDropPositionFromX(ranges, 50)).toBe(0);
    });

    it('should handle x values after last boundary', () => {
      const boundaries = [0, 100, 200];
      const validPositions = [0, 1, 3];
      const ranges = computeDropPositionRanges(boundaries, validPositions);

      expect(getDropPositionFromX(ranges, 300)).toBe(3);
      expect(getDropPositionFromX(ranges, 1000)).toBe(3);
    });

    it('should correctly map x values near the end of columns', () => {
      // Simulate dragging color (index 5) - valid positions: [0, 4, 5, 6, 8, 9]
      const boundaries = [0, 100, 200, 300, 400, 500, 600, 700, 800, 900];
      const validPositions = [0, 4, 5, 6, 8, 9];
      const ranges = computeDropPositionRanges(boundaries, validPositions);

      // X values between columns 8 and 9 should map to position 9 (end)
      // Column 8 is at x=800, column 9 would be at x=900
      expect(getDropPositionFromX(ranges, 850)).toBe(9);
      expect(getDropPositionFromX(ranges, 950)).toBe(9);
    });

    it('should map x values on right half of last column to end position', () => {
      // Simulate dragging color (index 5)
      const boundaries = [0, 100, 200, 300, 400, 500, 600, 700, 800, 900];
      const validPositions = [0, 4, 5, 6, 8, 9];
      const ranges = computeDropPositionRanges(boundaries, validPositions);

      // Right half of column 8 (x > 850) should map to position 9 (end)
      expect(getDropPositionFromX(ranges, 851)).toBe(9);
      expect(getDropPositionFromX(ranges, 899)).toBe(9);
    });

    it('should return end-of-table info when drop position is at end', () => {
      const { getDropPositionDisplayInfo } = require('../../main/resources/columnOrder');
      
      // When dropPosition equals n (end of table)
      const displayInfo = getDropPositionDisplayInfo(COLUMN_IDS, [0,100,200,300,400,500,600,700,800,900], 5, false, 9);
      
      expect(displayInfo.type).toBe('line');
      expect(displayInfo.isEndOfTable).toBe(true);
    });

    it('should calculate correct end-of-table position with page-relative boundaries', () => {
      const { getDropPositionDisplayInfo } = require('../../main/resources/columnOrder');
      
      // Simulate table starting at x=100, with 100px columns
      // Column 8 would span from x=800 to x=900
      const boundaries = [100, 200, 300, 400, 500, 600, 700, 800, 900];
      const displayInfo = getDropPositionDisplayInfo(COLUMN_IDS, boundaries, 5, false, 9);
      
      expect(displayInfo.type).toBe('line');
      expect(displayInfo.isEndOfTable).toBe(true);
      expect(displayInfo.boundaryIndex).toBe(8);
    });

    it('should return end-of-table flag for end position', () => {
      const { getDropPositionDisplayInfo, COLUMN_IDS } = require('../../main/resources/columnOrder');
      
      // With 9 columns, position 9 is "after the last column"
      const displayInfo = getDropPositionDisplayInfo(COLUMN_IDS, [0,100,200,300,400,500,600,700,800], 5, false, 9);
      
      expect(displayInfo.isEndOfTable).toBe(true);
    });
  });

  describe('getInsertIndex', () => {
    it('should return -1 when dropping at current position', () => {
      expect(getInsertIndex(COLUMN_IDS, 4, false, 4)).toBe(-1);
    });

    it('should return -1 when dropping at current position for any column', () => {
      // Color is at index 5
      expect(getInsertIndex(COLUMN_IDS, 5, false, 5)).toBe(-1);
    });

    it('should return -1 when dragging forward to adjacent position', () => {
      expect(getInsertIndex(COLUMN_IDS, 4, false, 5)).toBe(-1);
    });

    it('should return 4 when dragging backward to adjacent position (within category range)', () => {
      // Position 4 is within category range, so effectivePos = 5, insertIndex = 4
      expect(getInsertIndex(COLUMN_IDS, 5, false, 4)).toBe(4);
    });

    it('should return correct index when moving forward past multiple columns', () => {
      expect(getInsertIndex(COLUMN_IDS, 0, false, 8)).toBe(7);
    });

    it('should return correct index when moving backward', () => {
      expect(getInsertIndex(COLUMN_IDS, 8, false, 0)).toBe(0);
    });

    it('should move categories to end of range when dropping within category range', () => {
      // Dragging categories to position 2 (within range) should move to end of range
      // Categories at [0,1,2,3], dropPosition=2, range.end=3
      // effectivePos = 4, insertIndex = 4 - 4 = 0
      expect(getInsertIndex(COLUMN_IDS, 1, true, 2)).toBe(0);
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
