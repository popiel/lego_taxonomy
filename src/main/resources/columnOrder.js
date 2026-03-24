(function (root, factory) {
  if (typeof module === 'object' && module.exports) {
    module.exports = factory();
  } else {
    root.columnOrder = factory();
  }
}(typeof self !== 'undefined' ? self : this, function () {
  const COLUMN_IDS = ['category', 'category2', 'category3', 'category4', 'image', 'color', 'quantity', 'name', 'partNumber'];
  const CATEGORY_COLUMNS = ['category', 'category2', 'category3', 'category4'];

  function getColumnIndex(order, columnId) {
    return order.indexOf(columnId);
  }

  function isCategoryColumn(columnId) {
    return CATEGORY_COLUMNS.includes(columnId);
  }

  function getCategoryRange(order) {
    const indices = order
      .map((col, idx) => CATEGORY_COLUMNS.includes(col) ? idx : -1)
      .filter(idx => idx >= 0);
    if (indices.length === 0) return { start: -1, end: -1 };
    return { start: indices[0], end: indices[indices.length - 1] };
  }

  function getValidDropPositions(order, draggedColumnIndex, isDraggingCategory) {
    const n = order.length;
    const range = getCategoryRange(order);
    const validPositions = [];

    let draggedIndices;
    if (isDraggingCategory) {
      draggedIndices = [];
      for (let i = 0; i < n; i++) {
        if (CATEGORY_COLUMNS.includes(order[i])) {
          draggedIndices.push(i);
        }
      }
    } else {
      draggedIndices = [draggedColumnIndex];
    }

    const minDragged = Math.min(...draggedIndices);
    const maxDragged = Math.max(...draggedIndices);

    // Position 0: invalid if dragging first column or first category (when isDraggingCategory)
    const canDropAtStart = isDraggingCategory ? range.start > 0 : minDragged > 0;
    if (canDropAtStart) {
      validPositions.push(0);
    }

    // Positions between columns (1 to n-1)
    for (let pos = 1; pos < n; pos++) {
      const beforeIsDragged = draggedIndices.includes(pos - 1);
      const afterIsDragged = draggedIndices.includes(pos);
      
      // Position equals dragged column index means we're at the current position (before the dragged column)
      // This is always valid - it allows dropping back at the original spot
      const atCurrentPosition = draggedIndices.includes(pos) && !beforeIsDragged;
      
      // For non-category drags, exclude positions within category range
      // For category drags, exclude positions within the category range
      let withinCategoryRange = false;
      if (range.start !== -1) {
        withinCategoryRange = pos > range.start && pos <= range.end;
      }
      
      // Valid if:
      // - At the current position (before the dragged column), OR
      // - Neither boundary column is being dragged (not adjacent)
      const notAdjacentToDragged = !beforeIsDragged && !afterIsDragged;
      const isValid = atCurrentPosition || notAdjacentToDragged;
      
      if (isValid && !withinCategoryRange) {
        validPositions.push(pos);
      }
    }

    // Position n: invalid if dragging last column or last category (when isDraggingCategory)
    const canDropAtEnd = isDraggingCategory ? range.end < n - 1 : maxDragged < n - 1;
    if (canDropAtEnd) {
      validPositions.push(n);
    }

    return validPositions;
  }

  function computeDropPositionRanges(boundaryCoordinates, validPositions) {
    const ranges = [];
    const n = boundaryCoordinates.length;

    const midpoints = [];
    for (let i = 0; i < n - 1; i++) {
      midpoints.push((boundaryCoordinates[i] + boundaryCoordinates[i + 1]) / 2);
    }

    const lastColumnRight = boundaryCoordinates[n - 1] + (boundaryCoordinates[n - 1] - boundaryCoordinates[n - 2]);

    let lastX2 = -Infinity;

    for (let i = 0; i < validPositions.length; i++) {
      const dropPos = validPositions[i];
      let x2;

      if (i === validPositions.length - 1) {
        x2 = Infinity;
      } else {
        const nextDropPos = validPositions[i + 1];
        if (nextDropPos === 0) {
          x2 = midpoints[0];
        } else if (nextDropPos >= n) {
          x2 = lastColumnRight;
        } else if (nextDropPos === dropPos + 1) {
          x2 = boundaryCoordinates[nextDropPos - 1];
        } else {
          x2 = midpoints[nextDropPos - 1];
        }
      }

      ranges.push({
        x1: lastX2,
        x2: x2,
        dropPosition: dropPos
      });

      lastX2 = x2;
    }

    return ranges;
  }

  function getDropPositionFromX(ranges, x) {
    for (const range of ranges) {
      if (x >= range.x1 && x < range.x2) {
        return range.dropPosition;
      }
    }
    return ranges[ranges.length - 1].dropPosition;
  }

  function getDropPositionDisplayInfo(order, boundaryCoordinates, draggedColumnIndex, isDraggingCategory, dropPosition) {
    const n = order.length;
    const range = getCategoryRange(order);

    let draggedIndices;
    if (isDraggingCategory) {
      draggedIndices = [];
      for (let i = 0; i < n; i++) {
        if (CATEGORY_COLUMNS.includes(order[i])) {
          draggedIndices.push(i);
        }
      }
    } else {
      draggedIndices = [draggedColumnIndex];
    }

    const minDragged = Math.min(...draggedIndices);
    const maxDragged = Math.max(...draggedIndices);

    // Show box if at current position (before or after the block)
    const atBeforePosition = dropPosition === minDragged;
    const atAfterPosition = dropPosition === maxDragged + 1;

    if (atBeforePosition || atAfterPosition) {
      return { type: 'box', startBoundary: boundaryCoordinates[minDragged], endBoundary: boundaryCoordinates[maxDragged] };
    }

    // Show line at boundary
    let boundaryIndex;
    let isEndOfTable = false;
    if (dropPosition === 0) {
      boundaryIndex = 0;
    } else if (dropPosition === n) {
      boundaryIndex = n - 1;
      isEndOfTable = true;
    } else {
      boundaryIndex = dropPosition;
    }

    return { type: 'line', boundaryIndex: boundaryIndex, isEndOfTable: isEndOfTable };
  }

  function getInsertIndex(order, draggedColumnIndex, isDraggingCategory, dropPosition) {
    const n = order.length;
    const range = getCategoryRange(order);

    let draggedIndices;
    if (isDraggingCategory) {
      draggedIndices = [];
      for (let i = 0; i < n; i++) {
        if (CATEGORY_COLUMNS.includes(order[i])) {
          draggedIndices.push(i);
        }
      }
    } else {
      draggedIndices = [draggedColumnIndex];
    }

    if (draggedIndices.length === 0) return -1;

    const minDragged = Math.min(...draggedIndices);
    const maxDragged = Math.max(...draggedIndices);

    // Check if this is a no-op (dropping at current position)
    if (dropPosition === minDragged || dropPosition === maxDragged + 1) {
      return -1;
    }

    let insertIndex;

    if (isDraggingCategory) {
      // For category moves:
      // - If dropping at position 0, allow it
      // - If dropping within category range (where categories currently are), clamp to after range
      // - If dropping after category range, allow it
      let effectivePos;
      if (dropPosition === 0) {
        effectivePos = 0;
      } else if (dropPosition >= range.start && dropPosition <= range.end) {
        // Dropping within current category range - clamp to after range
        effectivePos = range.end + 1;
      } else {
        effectivePos = dropPosition;
      }
      
      if (effectivePos === 0) {
        insertIndex = 0;
      } else {
        insertIndex = effectivePos - draggedIndices.length;
      }
    } else {
      // For non-category moves
      let effectiveDropPosition = dropPosition;
      
      // If dropping within category range, move to end of category range
      if (dropPosition > range.start && dropPosition <= range.end) {
        effectiveDropPosition = range.end + 1;
      }
      
      if (effectiveDropPosition > maxDragged) {
        insertIndex = effectiveDropPosition - draggedIndices.length;
      } else {
        insertIndex = effectiveDropPosition;
      }
    }

    return insertIndex;
  }

  function moveToDropPosition(order, draggedColumnIndex, isDraggingCategory, dropPosition) {
    const n = order.length;
    const range = getCategoryRange(order);

    let draggedIndices;
    if (isDraggingCategory) {
      draggedIndices = [];
      for (let i = 0; i < n; i++) {
        if (CATEGORY_COLUMNS.includes(order[i])) {
          draggedIndices.push(i);
        }
      }
    } else {
      draggedIndices = [draggedColumnIndex];
    }

    if (draggedIndices.length === 0) return [...order];

    const insertIndex = getInsertIndex(order, draggedColumnIndex, isDraggingCategory, dropPosition);

    if (insertIndex === -1) {
      return [...order];
    }

    // Sort indices in descending order for removal
    const sortedIndices = [...draggedIndices].sort((a, b) => b - a);

    let newOrder = [...order];
    for (const idx of sortedIndices) {
      newOrder.splice(idx, 1);
    }

    // Insert at new position
    if (isDraggingCategory) {
      const columnsToInsert = draggedIndices.map(idx => order[idx]);
      newOrder.splice(insertIndex, 0, ...columnsToInsert);
    } else {
      newOrder.splice(insertIndex, 0, order[draggedColumnIndex]);
    }

    return newOrder;
  }

  function resetColumns() {
    return [...COLUMN_IDS];
  }

  return {
    COLUMN_IDS,
    CATEGORY_COLUMNS,
    getColumnIndex,
    isCategoryColumn,
    getCategoryRange,
    getValidDropPositions,
    computeDropPositionRanges,
    getDropPositionFromX,
    getDropPositionDisplayInfo,
    getInsertIndex,
    moveToDropPosition,
    resetColumns
  };
}));
